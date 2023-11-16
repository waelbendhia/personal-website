module PersonalWebsite.TH (embedGitCommitHash, embedRepositoryURL) where

import Language.Haskell.TH
import Relude
import System.Process

embedGitCommitHash :: Q Exp
embedGitCommitHash = do
    short <- runIO (readProcess "git" ["rev-parse", "--short", "HEAD"] [])
    long <- runIO (readProcess "git" ["rev-parse", "HEAD"] [])
    [|(short, long)|]

embedRepositoryURL :: Q Exp
embedRepositoryURL = do
    mRepoURL <- runIO $ lookupEnv "REPOSITORY_URL"
    let repoURL = fromMaybe "https://github.com/waelbendhia/s-vet" mRepoURL
    [|repoURL|]
