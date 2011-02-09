{-# LANGUAGE RecordWildCards #-}
-- This new version of BlogLiterately adds a few more options and tries to allow
-- the user to take advantage of the Pandoc syntax highlighting, or suppress
-- it.

module Main where

-- We need [Pandoc][] for parsing [Markdown][]:

import Text.Pandoc
import Text.Pandoc.Highlighting

-- And [hscolour][] for highlighting:

import Language.Haskell.HsColour(hscolour,Output(..))
import Language.Haskell.HsColour.Colourise(defaultColourPrefs)

-- To post to a blog, we need the [MetaWeblog][] API, which is an XML-RPC-based
-- protocol for interacting with blogs.
--
-- We'll use the Haskell XML-RPC library, [HaXR][], by Bjorn Bringert, (on
-- [hackage][hackage-haxr]). *Note: the latest version (as of this writing) of
-- HaXR on Hackage does not specify an upper bound in its dependency on HaXml, but
-- it is incompatible with the 1.19 versions of HaXml!  If you have HaXml-1.19.*
-- installed, you'll have to work around this.*

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

-- And it works that out I'll need some miscellaneous other stuff. Since I'm
-- writing a command line tool, I'll need to process the command line arguments, 
-- use GetOpt in base for that:

import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv)
import System.Exit ( exitSuccess, exitFailure )
import System.FilePath

-- I'm going to end up needing to parse and manipulate XHTML, so I'll use Malcolm
-- Wallace's [HaXml][] XML combinators:

import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Verbatim

import qualified System.IO.UTF8 as U
import qualified Data.ByteString as BS

import Control.Monad(liftM,unless)
import Text.XHtml.Transitional(showHtmlFragment)
import Text.ParserCombinators.Parsec
import Data.Monoid

import qualified Data.Map as M

import Config

-- The program will read in a literate Haskell file, use Pandoc to parse it as
-- markdown, and, if it is using hscolour to for the Haskell pieces, will use
-- hscolour to transform those.  Pandoc turns its input into a structure of type:
--
--     [haskell]
--     data Pandoc = Pandoc Meta [Block]
--
-- where a `Block` (the interesting bit, for my purposes) looks like:
--
--     [haskell]
--     -- | Block element.
--     data Block
--         = Plain [Inline]        -- ^ Plain text, not a paragraph
--         | Para [Inline]         -- ^ Paragraph
--         | CodeBlock Attr String -- ^ Code block (literal) with attributes
--         | RawBlock Bool String  -- ^ Raw HTML block (literal)
--         | BlockQuote [Block]    -- ^ Block quote (list of blocks)
--         | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
--                                 -- and a list of items, each a list of blocks)
--         | BulletList [[Block]]  -- ^ Bullet list (list of items, each
--                                 -- a list of blocks)
--         | DefinitionList [([Inline],[Block])]  -- ^ Definition list
--                                 -- (list of items, each a pair of an inline list,
--                                 -- the term, and a block list)
--         | Header Int [Inline]   -- ^ Header - level (integer) and text (inlines)
--         | HorizontalRule        -- ^ Horizontal rule
--         | Table [Inline] [Alignment] [Double] [[Block]] [[[Block]]]  -- ^ Table,
--                                 -- with caption, column alignments,
--                                 -- relative column widths, column headers
--                                 -- (each a list of blocks), and rows
--                                 -- (each a list of lists of blocks)
--         | Null                  -- ^ Nothing
--         deriving (Eq, Read, Show, Typeable, Data)
--
-- The literate Haskell that Pandoc finds in a file ends up in various `CodeBlock`
-- elements of the `Pandoc` document.  Other code can also wind up in `CodeBlock`
-- elements -- normal markdown formatted code.  The `Attr` component has
-- metadata about what's in the code block:
--
--     [haskell]
--     type Attr = (String, -- code block identifier
--                      [String], -- list of code classes
--                      [(String, String)]) -- name/value pairs
--
-- Thanks to some feedback from the Pandoc author, John MacFarlane, I learned that
-- the CodeBlock *may* contain markers about the kind of code contained within the
-- block.  LHS (bird-style or LaTex style) will always have an `Attr` of the form
-- `("",["sourceCode","haskell"],[])`, and other `CodeBlock`
-- elements are the markdown code blocks *may* have an identifier, classes, or 
-- key/value pairs.  Pandoc captures this info when the file contains code blocks
-- in the delimited (rather than indented) format, which allows an optional 
-- meta-data specification, e.g.
--
-- ~~~~~~~~~~~
-- ~~~~~~~ { .bash }
-- x=$1
-- echo $x
-- ~~~~~~~
-- ~~~~~~~~~~~
--
-- Although Pandoc supports the above format for marking code blocks (and 
-- annotating the kind of code within the block) I'll also keep my notation as
-- another option for use with indented blocks, i.e. if you write:
--
-- <pre><code>
--     [haskell]
--     foo :: String -> String
-- </code></pre>
--
-- it is a Haskell block.  If it looks like something else, e.g.
--
-- <pre><code>
--     [cpp]
--     cout << "Hello World!";
-- </code></pre>
--
-- or
-- <pre><code>
--     [other]
--     foo bar baz
-- </pre></code>
--
-- If highlighting-kate is specified for highlighting Haskell blocks, the distinction
-- between the literate blocks and the delimited blocks is lost (this is simply how
-- the Pandoc highlighting module currently works).
--
-- I'll adopt the rule that if you specify a class or
-- classes using Pandoc's delimited code block syntax, I'll assume that there is 
-- no additional tag within the block in Blog Literately syntax.  I still need my
-- `unTag` function to parse the code block.

unTag :: String -> (String, String)
unTag s = either (const ("",s)) id $ parse tag "" s
   where tag = do
             tg <- between (char '[') (char ']') $ many $ noneOf "[]"
             skipMany $ oneOf " \t"
             (string "\r\n" <|> string "\n")
             txt <- many $ anyToken
             eof
             return (tg,txt)

-- To highlight the syntax using hscolour (which produces HTML), I'm going to
-- need to transform the `String` from a `CodeBlock` element to a `String` 
-- suitable for the `RawBlock` element (because the hscolour library transforms
-- Haskell text to HTML). Pandoc strips off the prepended &gt; characters from the
-- literate Haskell, so I need to put them back, and also tell hscolour whether the
-- source it is colouring is literate or not.  The hscolour function looks like:
--
--     [haskell]
--     hscolour :: Output      -- ^ Output format.
--              -> ColourPrefs -- ^ Colour preferences...
--              -> Bool        -- ^ Whether to include anchors.
--              -> Bool        -- ^ Whether output document is partial or complete.
--              -> String      -- ^ Title for output.
--              -> Bool        -- ^ Whether input document is literate haskell
--              -> String      -- ^ Haskell source code.
--              -> String      -- ^ Coloured Haskell source code.
--
-- Since I still don't like the `ICSS` output from hscolour, I'm going to provide
-- two options for hscolouring to users: one that simply uses hscolour's `CSS`
-- format, so the user can provide definitions in their blog's stylesheet to
-- control the rendering, and a post-processing option to transform the `CSS`
-- class-based rendering into a inline style based rendering (for people who can't
-- update their stylesheet).  `colourIt` performs the initial transformation:

colourIt literate srcTxt = 
    hscolour CSS defaultColourPrefs False True "" literate srcTxt'
    where srcTxt' | literate = prepend srcTxt
                  | otherwise = srcTxt
  
-- Prepending the literate Haskell markers on the source:

prepend s = unlines $ map ("> " ++) $ lines s

-- Hscolour uses HTML `span` elements and CSS classes like 'hs-keyword' or 
-- `hs-keyglyph` to markup Haskell code.  What I want to do is take each marked 
-- `span` element and replace the `class` attribute with an inline `style` element
-- that has the markup I want for that kind of source.  I've rethought the style 
-- preferences type, and think it will be simpler, and more general, as just a list
-- of name/value pairs:

type StylePrefs = [(String,String)]

-- The default style that produces something like what the source listings
-- on Hackage look like is now:

defaultStylePrefs = [
    ("hs-keyword","color: blue; font-weight: bold;")
  , ("hs-keyglyph","color: red;")
  , ("hs-layout","color: red;")
  , ("hs-comment","color: green;")
  , ("hs-conid", "")
  , ("hs-varid", "")
  , ("hs-conop", "")
  , ("hs-varop", "")
  , ("hs-str", "color: teal;")
  , ("hs-chr", "color: teal;")
  , ("hs-number", "")
  , ("hs-cpp", "")
  , ("hs-selection", "")
  , ("hs-variantselection", "")
  , ("hs-definition", "")]

-- I can read these preferences in from a file using the `Read` instance for
-- `StylePrefs`.  I could handle errors better, but this should work:

getStylePrefs "" = return defaultStylePrefs
getStylePrefs fname = liftM read (U.readFile fname)

-- Hscolour produces a `String` of HTML.  To 'bake' the styles into
-- the HTML it, we need to parse it, manipulate it
-- and then re-render it as a `String`.  Use HaXml to do all of this:

bakeStyles :: StylePrefs -> String -> String
bakeStyles prefs s =  verbatim $ filtDoc (xmlParse "bake-input" s) where
    -- filter the document (an Hscoloured fragment of Haskell source)
    filtDoc (Document p s e m) =  c where
        [c] = filts (CElem e noPos)
    -- the filter is a fold of individual filters for each CSS class
    filts = mkElem "pre" [(foldXml $ foldl o keep $ map filt prefs) `o` replaceTag "code"]
    -- an individual filter replaces the attributes of a tag with
    -- a style attribute when it has a specific 'class' attribute.
    filt (cls,style) =
        replaceAttrs [("style",style)] `when`
            (attrval $ ("class",AttValue [Left cls]))

-- Highlighting-Kate uses &lt;br/> in code blocks to indicate newlines.  WordPress
-- (if not other software) chooses to strip them away when found in &lt;pre> sections
-- of uploaded HTML.  So need to turn them back to newlines.

replaceBreaks :: String -> String
replaceBreaks s = verbatim $ filtDoc (xmlParse "input" s) where
   -- filter the document (a highlighting-kate hitlited fragment of
   -- haskell source
   filtDoc (Document p s e m) = c where
       [c] = filts (CElem e noPos)
   filts = foldXml (literal "\n" `when` tag "br")

-- Note to self: the above is a function that could be made better in a 
-- few ways and then factored out into a library.  A way to handle the 
-- above would be to allow the preferences to be specified as an actual CSS
-- style sheet, which then would be baked into the HTML.  Such a function
-- could be separately useful, and could be used to 'bake' in the
-- highlighting-kate styles.
--
-- To completely colourise/highlight a `CodeBlock` we now can create a function
-- that transforms a `CodeBlock` into a `RawBlock` block, where the content contains
-- marked up Haskell (possibly with literate markers), or marked up non-Haskell, if
-- highlighting of non-Haskell has been selected.

colouriseCodeBlock :: HsHighlight -> Bool -> Block -> Block
colouriseCodeBlock hsHilite otherHilite b@(CodeBlock attr@(_,classes,_) s) =
    if tag == "haskell" || haskell
        then case hsHilite of
            HsColourInline style -> 
                RawBlock "html" $ bakeStyles style $ colourIt lit src
            HsColourCSS -> RawBlock "html" $ colourIt lit src
            HsNoHighlight -> RawBlock "html" $ simpleHTML hsrc
            HsKate -> if null tag 
                then myHiliteK attr hsrc
                else myHiliteK ("",tag:classes,[]) hsrc
        else if otherHilite
            then case tag of
                "" -> myHiliteK attr src
                t -> myHiliteK ("",[t],[]) src
            else RawBlock "html" $ simpleHTML src
    where (tag,src) = if null classes then unTag s else ("",s)
          hsrc = if lit then prepend src else src
          lit = False -- "sourceCode" `elem` classes (avoid ugly '>')
          haskell = "haskell" `elem` classes
          simpleHTML s = "<pre><code>" ++ s ++ "</code></pre>"
          myHiliteK attr s = case highlightHtml True attr s of
              Left _ -> RawBlock "html" $ simpleHTML s
              Right html -> RawBlock "html" $ replaceBreaks $ showHtmlFragment html
colouriseCodeBlock _ _ b = b

colourisePandoc :: HsHighlight -> Bool -> Pandoc -> Pandoc
colourisePandoc hsHilite otherHilite (Pandoc m blocks) = 
    Pandoc m $ map (colouriseCodeBlock hsHilite otherHilite) blocks

data FileMarkup = Markdown
                | RST
    deriving (Show)

instance Monoid FileMarkup where
  mempty = Markdown
  mappend = const

-- Transforming a complete input document string to an HTML output string:

parseDocument :: FileMarkup -> String -> Either String (String, Maybe String, Pandoc) 
parseDocument markup s = do
  title <- if null . docTitle $ meta
            -- we need a title in the document
            then Left "Please add a :Title: to your document"
            -- render the title into text
            else return $ writeHtmlString defaultWriterOptions (Pandoc (Meta [] [] []) ([Plain (docTitle meta)]))

  let (postId, decl') = extractPostID decl

  return (title, postId, Pandoc (Meta [] [] []) decl')
  where
    doc@(Pandoc meta decl) = pandoc_parser parseOpts $ fixLineEndings s
    pandoc_parser = case markup of
                     Markdown -> readMarkdown
                     RST      -> readRST
    parseOpts = defaultParserState { 
        stateLiterateHaskell = True }
    -- pandoc is picky about line endings
    fixLineEndings [] = []
    fixLineEndings ('\r':'\n':cs) = '\n':fixLineEndings cs
    fixLineEndings (c:cs) = c:fixLineEndings cs

    -- extract the postid from the document, if there is any
    -- it's quite picky, must be on the format:
    -- :PostID: 34
    extractPostID :: [Block] -> (Maybe String, [Block])
    extractPostID ((DefinitionList [([Str "PostID"], [[Para [Str postId]]])]) : rest) = (Just postId, rest)
    extractPostID (other : rest) = let (postId,decl') = extractPostID rest in (postId, other:decl')
    extractPostID [] = (Nothing, [])

xformDoc :: HsHighlight -> Bool -> Pandoc -> String
xformDoc hsHilite otherHilite doc =
    showHtmlFragment 
    $ writeHtml writeOpts -- from Pandoc
    $ colourisePandoc hsHilite otherHilite
    $ doc
  where
     writeOpts = defaultWriterOptions {
        --writerLiterateHaskell = True,
        writerReferenceLinks = True }

-- The metaWeblog API defines a `newPost` and  `editPost` procedures that look
-- like:
--
--     [other]
--     metaWeblog.newPost (blogid, username, password, struct, publish)
--         returns string
--     metaWeblog.editPost (postid, username, password, struct, publish)
--         returns true
--
-- For my blog (a WordPress blog), the `blogid` is just `default`.  The user
-- name and password are simply strings, and `publish` is a flag indicating whether
-- to load the post as a draft, or to make it public immediately.  The `postid` is
-- an identifier string which is assigned when you initially create a post. The
-- interesting bit is the `struct` field, which is an XML-RPC structure defining 
-- the post along with some meta-data, like the title.  I want be able to provide
-- the post body, a title, and a list of categories.  The for the
-- body and title, we could just let HaXR convert the values automatically
-- into the XML-RPC `Value` type, since they all have the same Haskell type
-- (`String`) and thus can be put into a list.  But the categories are a list of
-- strings, so we need to explicitly convert everything to a `Value`, then combine:

mkPost title text categories keywords = 
    cats ++ [("title",toValue title),("description",toValue text)] ++ tags
    where cats = if null categories then [] 
              else [("categories",toValue categories)]
          tags = if null keywords then [] 
              else [("mt_keywords",toValue keywords)]


-- The HaXR library exports a function for invoking XML-RPC procedures:
--
--     [haskell]
--     remote :: Remote a => 
--         String -- ^ Server URL. May contain username and password on
--                --   the format username:password\@ before the hostname.
--            -> String -- ^ Remote method name.
--            -> a      -- ^ Any function 
--          -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) => 
--                      -- t1 -> ... -> tn -> IO r@
--
-- The function requires an URL and a method name, and returns a function of type
-- `Remote a => a`.  Based on the instances defined for `Remote`, any function
-- with zero or more parameters in the class `XmlRpcType` and a return type of
-- `XmlRpcType r => IO r` will work, which means you can simply 'feed' `remote`
-- additional arguments as required by the remote procedure, and as long as you
-- make the call in an IO context, it will typecheck.  So to call the
-- `metaWeblog.newPost` procedure, I can do something like:

postIt :: String -> String -> String -> String -> String -> String 
    -> [String] -> [String] -> Bool -> IO String
postIt url blogId user password title text cats keywords publish =
    remote url "metaWeblog.newPost" blogId user password 
        (mkPost title text cats keywords) publish

-- To update (replace) a post, the function would be:

updateIt :: String -> String -> String -> String -> String -> String 
    -> [String] -> [String] -> Bool -> IO Bool
updateIt url postId user password title text cats keywords publish =
    remote url "metaWeblog.editPost" postId user password
        (mkPost title text cats keywords) publish

-- There are four modes of Haskell highlighting:

data HsHighlight = HsColourInline { hsStylePrefs :: StylePrefs }
    | HsColourCSS | HsKate | HsNoHighlight
    deriving (Show,Eq)

-- And two modes for other code (off or on!).
--
-- We can figure out if Pandoc is linked with highlighting-kate (we
-- won't show the kate-related options if it isn't):

noKate = null defaultHighlightingCss

-- To create a command line program,  I can capture the command line controls in a type:

data BlogLiterately = BlogLiterately {
       showHelp :: Bool,
       showVersion :: Bool,
       verbosity :: Verbosity,
       mode :: Maybe Mode,
       style :: String,    -- name of a style file
       hshighlight :: HsHighlight,
       highlightOther :: Bool, -- use highlight-kate to highlight other code
       publish :: Bool,    -- an indication of whether the post should be
                               -- published, or loaded as a draft
       categories :: [String], --
       keywords :: [String], -- tag list
       blogid :: String,   -- blog-specific identifier (e.g. for blogging
                               -- software handling multiple blogs)
       postid :: Maybe String,   -- id of a post to updated
       file :: String,     -- file to post
       file_markup :: Maybe FileMarkup
    } deriving (Show)

defaultBlogLiterately :: BlogLiterately
defaultBlogLiterately = BlogLiterately {
  showHelp = False,
  showVersion = False,
  verbosity = NormalVerbosity,
  mode = Nothing,
  style = "",
  hshighlight = HsColourInline defaultStylePrefs,
  highlightOther = False,
  publish = False,
  categories = [],
  keywords = [],
  blogid = "default",
  postid = Nothing,
  file = "",
  file_markup = Nothing
  }

data Verbosity = LowVerbosity
               | NormalVerbosity
               | HighVerbosity
               deriving (Eq,Show)

data Mode = Standalone
          | Online { blog :: String
                   , user :: String
                   , password :: String
                   }
          deriving (Eq,Show)

options :: [OptDescr (BlogLiterately -> BlogLiterately)]
options =
  [ Option "?" ["help"] (NoArg (\opts -> opts { showHelp = True })) "Show usage information"
  , Option "V" ["version"] (NoArg (\opts -> opts { showVersion = True})) "Show version information"
-- not used
--  , Option "v" ["verbose"] (NoArg (\opts -> opts { verbosity = HighVerbosity })) "Higher verbosity"
--  , Option "q" ["quiet"] (NoArg (\opts -> opts { verbosity = LowVerbosity })) "Lower verbosity"
  , Option "s" ["standalone"] (NoArg (\opts -> opts { mode = Just Standalone })) "run standalone; html goes to stdout, it's not uploaded"
  , Option "" ["style"] (ReqArg (\o opts -> opts { style = o}) "FILE") "Style Specification (for --hscolour-icss)"
  , Option ""  ["hscolour-icss"] (NoArg (\opts -> opts { hshighlight = HsColourInline defaultStylePrefs }))
                                                                       "hilight haskell: hscolour, inline style (default)"
  , Option ""  ["hscolour-css"] (NoArg (\opts -> opts { hshighlight = HsColourCSS })) "hilight haskell: hscolour, separate stylesheet"
  , Option ""  ["hs-nohilight"] (NoArg (\opts -> opts { hshighlight = HsNoHighlight })) "no haskell hilighting"
  ] ++ concat [[ -- only if we have kate
    Option ""  ["hs-kate"] (NoArg (\opts -> opts { hshighlight = HsKate })) "hilight other code with highlighting-kate"
  , Option ""  ["other-code-kate"] (NoArg (\opts -> opts { highlightOther = True })) "hilight other code with highlighting-kate"
  ] | not noKate] ++ [
    Option ""  ["publish"] (NoArg (\opts -> opts { publish = True })) "Publish post (otherwise it's uploaded as a draft)"
  , Option ""  ["category"] (ReqArg (\v opts -> opts { categories = categories opts ++ [v] }) "VALUE") "post category (can specify more than one)"
  , Option ""  ["tag"] (ReqArg (\v opts -> opts { keywords = keywords opts ++ [v] }) "VALUE") "set tag (can specify more than one)"
  , Option "b" ["blogid"] (ReqArg (\v opts -> opts { blogid = v }) "VALUE") "Blog specific identifier"
  , Option ""  ["postid"] (ReqArg (\v opts -> opts { postid = Just v }) "VALUE") "Post to replace (override value from document)"
  , Option "m" ["markup"] (ReqArg (\v opts -> opts { file_markup = Just (to_markup v) }) "VALUE") "File markup language: 'markdown' (default) or 'rst'"
  ]
  where to_markup :: String -> FileMarkup
        to_markup s = case s of
                          "markdown" -> Markdown
                          "md"       -> Markdown
                          "rst"      -> RST
                          _          -> error "bad markup"

extToMarkup :: String -> Maybe FileMarkup
extToMarkup file = case takeExtension file of
                    ".md" -> Just Markdown
                    ".rst" -> Just RST
                    _ -> Nothing

-- The main blogging function uses the information captured in the `BlogLiterately`
-- type to read the style preferences, read the input file and transform it, and
-- post it to the blog:
blogLiterately :: BlogLiterately -> IO ()
blogLiterately (BlogLiterately _ _ _ (Just mode) style hsmode other pub cats keywords blogid cmd_postid file (Just markup)) = do
    prefs <- getStylePrefs style
    let hsmode' = case hsmode of
            HsColourInline _ -> HsColourInline prefs
            _ -> hsmode
    fileContent <- U.readFile file

    case parseDocument markup fileContent of
      Left err -> putStrLn $ err
      Right (title, doc_postid, doc) -> do
        let html = xformDoc hsmode' other doc
        let postid = maybe doc_postid Just cmd_postid -- document postid can be overridden by command line
        case mode of
          Standalone -> do
            putStrLn $ "<-- Title: " ++ title ++ " -->"
            putStrLn $ "<-- PostID: " ++ maybe "none" id postid ++ " -->"
            putStr html
          Online {..} ->
            case postid of
              Nothing -> do
                 newpostid <- postIt blog "" user password title html cats keywords pub
                 putStrLn $ "Success!"
                 putStrLn $ "Please edit your document to include the new postid!"
                 putStrLn $ ":PostID: " ++ newpostid
              Just postidStr -> do
                 result <- updateIt blog postidStr user password title html cats keywords pub
                 if result
                  then putStrLn $ "Success!"
                  else putStrLn "update failed!"
blogLiterately _ = die "blogLiterately: internal error"

main :: IO ()
main = do
  bl <- getArgs >>= parseArgs
  case mode bl of
    Nothing -> printUsage >> exitSuccess
    Just _ -> blogLiterately bl { file_markup = mconcat [file_markup bl, extToMarkup (file bl), Just Markdown] }

get_auth_info :: BlogName -> IO BlogCred
get_auth_info blog_name =
    do home <- getEnv "HOME"
       let config_file = home </> ".blogliterately" </> "config"
       -- TODO: check permissions 600
       yet <- doesFileExist config_file
       if yet then return ()
              else do putStrLn ("ERROR: config file" ++ config_file ++ " not found.\n")
                      exitFailure
       config <- BS.readFile config_file
       blogs <- case parse_config config_file config of
                      Right b -> return b
                      Left  e -> do putStrLn $ e
                                    exitFailure
       case M.lookup blog_name blogs of
           Nothing -> do putStrLn $ "ERROR: " ++ blog_name ++ " was not found in config"
                         exitFailure
           Just b  -> return b

parseArgs :: [String] -> IO BlogLiterately
parseArgs args = do
  case runFlags $ getOpt Permute options args of
    (opts, _, _) | showHelp opts -> printUsage >> exitSuccess
                 | showVersion opts -> putStrLn copyright >> exitSuccess
    (opts, [file], []) | mode opts == Just Standalone -> do
      return opts { file = file }       
    (opts, [blog_name, file], []) -> do
      BlogCred url _api user password <- get_auth_info blog_name
      return opts { mode = Just (Online url user password), file = file }
    (_, _, err) | not (null err) -> die (unlines err)
    _ -> printUsage >> exitSuccess
 
 where accum flags = foldr (flip (.)) id flags defaultBlogLiterately
       runFlags (opts, nonOpts, errs) = (accum opts, nonOpts, errs)

copyright :: String
copyright = "BlogLiterately v0.3, (C) Robert Greayer 2010\n" ++
            "This program comes with ABSOLUTELY NO WARRANTY\n"

usage :: String -> String
usage prg = unlines
              [ ""
              , "Usage:"
              , ""
              , "  " ++ prg ++ " [ --standalone | BLOG_NAME ] [options] <file>"
              , ""
              , "Fields in the RST file:"
              , "(required)  :Title: the title for the blog post"
              , "(optional)  :PostID: 123"
              , ""
              , "The PostID can be overriden via the command line with --postid=VALUE, see below."
              , ""
              , "Options:"
              ]

die :: String -> IO a
die str = putStrLn str >> exitFailure

printUsage :: IO ()
printUsage = putStrLn (usageInfo (copyright ++ usage "BlogLiterately") options)

-- I can run it to get some help:
--
-- [markdown]: http://daringfireball.net/projects/markdown/
-- [pandoc]: http://johnmacfarlane.net/pandoc/ "Pandoc"
-- [hackage]: http://hackage.haskell.org/packages/hackage.html
-- [haddock]: http://www.haskell.org/haddock/
-- [hscolour]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
-- [metaweblog]: http://www.xmlrpc.com/metaWeblogApi
-- [haxr]: http://www.haskell.org/haxr/
-- [hackage-haxr]: http://hackage.haskell.org/package/haxr
-- [cmdargs]: http://community.haskell.org/~ndm/cmdargs/
-- [haxml]: http://www.cs.york.ac.uk/fp/HaXml/

