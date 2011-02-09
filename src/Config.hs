module Config
    ( BlogName
    , BlogCred(..)
    , BlogAPI(..)
    , parse_config
    , test
    ) where

import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

-- Blog URL or alias
type BlogName = String

data BlogAPI = MetaWeblog
    deriving Show

data BlogCred = BlogCred { blog_url      :: String -- rpc URL
                         , blog_api      :: BlogAPI
                         , blog_user     :: String
                         , blog_passowrd :: String
                         }
                deriving Show

type BlogCreds = M.Map BlogName BlogCred

-- TODO: read string in format:
--         foo
--         "ba r"
string_parser :: Parser String
string_parser = manyTill anyChar (char '\n' <|> (eof >> return '\n'))

proto_parser :: Parser BlogAPI
proto_parser =
  string "api" >> many1 space >>
  choice
    [ try (string "metaweblog") >> newline >> return MetaWeblog
    ,     (string "wordpress" ) >> newline >> return MetaWeblog
    ]

-- gets key value-string
-- try is not very efficient, but we get nicer errors
field_parser :: String -> Parser String
field_parser field = try (do _ <- string field >> many1 space
                             string_parser)

blog_entry_parser :: Parser (BlogCred, [BlogName])
blog_entry_parser =
    do blog      <- field_parser "blog"       <?> "blog <name>"
       let b_err s = concat ["'", blog, "':", s]
       url       <- field_parser "url"        <?> b_err "url <name>"
       proto     <- proto_parser              <?> b_err "api { wordpress | metaweblog }"
       user      <- field_parser "user"       <?> b_err "user <name>"
       password  <- field_parser "password"   <?> b_err "password <name>"

       aliases <- many (field_parser "alias"  <?> b_err "alias <name>")

       return $ (BlogCred url proto user password, blog:aliases)

-- returns an error or creds
parse_config :: String -> String -> Either String BlogCreds
parse_config file_name file_contents =
  case parse (many1 blog_entry_parser >>= \bs -> eof >> return bs) file_name file_contents of
    Left err -> Left (show err)
    Right blogs ->
      let flatten = [ (alias, cred)
                    | (cred, aliases) <- blogs
                    , alias <- nub aliases ]

      -- check for duplicate aliases
      in case map head $ filter (\lst -> length lst > 1) . groupBy (==) . sort . map fst $ flatten of
          [] -> Right $ M.fromList flatten
          dups -> Left $ "Found duplicate aliases: " ++ unwords dups

test :: IO ()
test = do print $ parse blog_entry_parser "test1" $ unlines [ "blog foo"
                                                            , "url u1"
                                                            , "api wordpress"
                                                            , "user u"
                                                            , "password p"
                                                            , "alias foo"
                                                            , "alias bar"
                                                            ]
          print $ parse_config            "test2" $ unlines [ "blog foo1"
                                                            , "url u1"
                                                            , "api wordpress"
                                                            , "user u"
                                                            , "password p"
                                                            , "alias foo1"
                                                            , "alias bar2"
                                                            , "blog foo2"
                                                            , "url u1"
                                                            , "api wordpress"
                                                            , "user u3"
                                                            , "password p3"
                                                            , "alias foo3"
                                                            , "alias bar4"
                                                            ]
