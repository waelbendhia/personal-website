module PersonalWebsite.TH (
    embedGitCommitHash,
    embedRepositoryURL,
    myGitHub,
    myLinkedIn,
) where

import Language.Haskell.TH
import Relude
import System.Process

embedGitCommitHash :: Q Exp
embedGitCommitHash = do
    short <- runIO (readProcess "git" ["rev-parse", "--short", "HEAD"] [])
    long <- runIO (readProcess "git" ["rev-parse", "HEAD"] [])
    [|(short, long)|]

myLinkedIn :: Q Exp
myLinkedIn = do
    mLinkedInID <- runIO $ lookupEnv "LINKEDIN_ID"
    let linkedInID = fromMaybe "wael-ben-dhia-39536613a" mLinkedInID
    [|"https://www.linkedin.com/in/" <> linkedInID|]

myGitHub :: Q Exp
myGitHub = do
    mUsername <- runIO $ lookupEnv "GITHUB_USERNAME"
    let username = fromMaybe "waelbendhia" mUsername
    [|"https://github.com/" <> username|]

embedRepositoryURL :: Q Exp
embedRepositoryURL = do
    mRepoURL <- runIO $ lookupEnv "REPOSITORY"
    let repoURL = fromMaybe "personal-website" mRepoURL
    [|$myGitHub <> "/" <> repoURL|]
