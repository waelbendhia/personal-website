module PersonalWebsite.Toys.API where

import PersonalWebsite.Colors
import Relude
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html

data SeedParam = Seed !ColorSeed | IncorrectSeed !Text

instance ToHttpApiData SeedParam where
    toUrlPiece (IncorrectSeed t) = t
    toUrlPiece (Seed t) = toText t

instance FromHttpApiData SeedParam where
    parseUrlPiece t = case Seed <$> parseUrlPiece t of
        Left _ -> Right (IncorrectSeed t)
        x -> x

type ColorGeneratorAPI =
    "toys"
        :> "palette-generator"
        :> QueryParam "seed" SeedParam
        :> Get '[HTML] Html

type ListToysAPI = "toys" :> Get '[HTML] Html

type ToysAPI = ColorGeneratorAPI :<|> ListToysAPI
