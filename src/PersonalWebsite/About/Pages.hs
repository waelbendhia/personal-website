module PersonalWebsite.About.Pages (aboutPage, ParsedCV (..)) where

import qualified Clay as C
import Data.Aeson.Optics
import Data.List (nub)
import Data.Yaml
import Optics
import Polysemy
import Polysemy.Input as P
import Relude hiding (div)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.XHtml5

newtype ParsedCV = ParsedCV Value

pageStyle :: C.Css
pageStyle = do
    ".about-body" C.? do
        C.maxWidth (C.px 768)
        C.margin (C.px 0) C.auto (C.px 0) C.auto
        C.div C.<? C.p C.<? do
            C.marginTop (C.rem 0.25)
            C.important $ C.marginBottom (C.rem 0.5)
        ".header-row" C.? do
            C.display C.grid
            C.gridTemplateColumns $ replicate 3 (C.fr 1)
            (C.h4 <> C.h3) C.<? do
                C.marginBottom (C.rem 0.5)
                C.marginTop (C.rem 1)
                C.lastOfType C.& do "justify-self" C.-: "flex-end"
                C.firstOfType C.& do "justify-self" C.-: "flex-start"
                "justify-self" C.-: "center"
                C.marginRight (C.px 0)
                C.marginLeft (C.px 0)

data Project = Project
    { title :: !Text
    , url :: !(Maybe Text)
    , repository :: !(Maybe Text)
    , outcome :: !(Maybe Text)
    , description :: !Text
    , technologies :: ![Text]
    }

projectFromValue :: Value -> Project
projectFromValue v =
    Project
        { title = fromMaybe "" $ v ^? key "title" % _String
        , url = v ^? key "url" % _String
        , repository = v ^? key "repository" % _String
        , outcome = v ^? key "outcome" % _String
        , description = fromMaybe "" $ v ^? key "description" % _String
        , technologies = v ^.. key "technologies" % _Array % traversed % _String
        }

workExperience :: Value -> Html
workExperience cv = (div ! A.class_ "work-experience") do
    h2 "Work Experience"
    forM_ wes \we -> do
        (div ! A.class_ "header-row") do
            let startDate = fromMaybe "" $ we ^? key "start" % _String
                endDate = fromMaybe "Present" $ we ^? key "end" % _String
            h3 $ text $ fromMaybe "" $ we ^? key "jobTitle" % _String
            h3 $ text $ fromMaybe "" $ we ^? key "company" % _String
            h3 $ text $ startDate <> " - " <> endDate
        p $ text $ fromMaybe "" $ we ^? key "roleDescription" % _String
  where
    wes = cv ^.. key "workExperience" % values

education :: Value -> Html
education cv = div do
    h2 "Education"
    forM_ edus \edu -> do
        (div ! A.class_ "header-row") do
            let startDate =
                    maybe "" (show . floor @_ @Int) (edu ^? key "start" % _Number)
                endDate =
                    maybe
                        "Present"
                        (show . floor @_ @Int)
                        (edu ^? key "end" % _Number)
            h3 $ text $ fromMaybe "" $ edu ^? key "city" % _String
            h3 $ text $ fromMaybe "" $ edu ^? key "school" % _String
            h3 $ text $ startDate <> " - " <> endDate
        p $ text $ fromMaybe "" $ edu ^? key "degree" % _String
  where
    edus = cv ^.. key "education" % values

projects :: Maybe Text -> Value -> Html
projects selectedTag cv = div do
    h2 "Projects"
    div do
        forM_ selectedTag \t ->
            div $ text $ "Showing only projects using '" <> t <> "'."
        br
        "Filter by technology:"
        br
        sequence_
            $ intersperse " "
            $ (a ! A.href "?") "Any"
            : (filterTag <$> techs)
    h3 "Professional Projects"
    mapM_ renderProject proProjects
    h3 "Personal Projects"
    mapM_ renderProject personalProjects
  where
    techs =
        sort
            $ nub
            $ cv
            ^.. key "workExperience"
            % values
            % key "projects"
            % _Array
            % traversed
            % key "technologies"
            % _Array
            % traversed
            % _String
    filterTag t = a ! A.href ("?tag=" <> fromString (toString t)) $ text t
    filterProjects =
        selectedTag & maybe
            id
            \t -> filter (\(Project _ _ _ _ _ ts) -> t `elem` ts)
    proProjects =
        filterProjects
            $ projectFromValue
            <$> (cv ^.. key "workExperience" % values % key "projects" % values)
    personalProjects =
        filterProjects
            $ projectFromValue
            <$> (cv ^.. key "personalProjects" % values)
    renderProject (Project t url' repo o d _) = do
        (div ! A.class_ "header-row") do
            h4 $ case url' of
                Just u' ->
                    (a ! A.href (fromString $ toString u') ! A.target "blank")
                        $ text t
                Nothing -> text t
            div pass
            forM_ repo \repo' ->
                h4
                    $ (a ! A.href (fromString $ toString repo') ! A.target "blank")
                        "Repository"
        p $ text d
        mapM_ (p . text) o

aboutPage :: (Members '[Input ParsedCV] r) => Maybe Text -> Sem r Html
aboutPage selectedTag = do
    ParsedCV cv <- P.input
    pure do
        style $ toMarkup $ C.render pageStyle
        (div ! A.class_ "about-body") do
            p do
                "You can "
                a
                    ! A.href "/public/wael-ben-dhia.pdf"
                    ! customAttribute "download" "wael-ben-dhia.pdf"
                    $ "download a neat résumé"
                " or scroll down to read the entire CV."
            div do
                h2 "Summary"
                mapM_
                    (mapM_ (p . fromString . toString) . lines)
                    (cv ^? key "generalInfo" % key "about" % key "short" % _String)
            workExperience cv
            education cv
            projects selectedTag cv
