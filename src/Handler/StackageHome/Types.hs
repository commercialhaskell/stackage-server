module Handler.StackageHome.Types (
    ApiSnapshotName(..),
) where

import ClassyPrelude.Yesod
import Types

-- | Combining SnapshotBranch and SnapName. Prevents needing overlapping routes
-- for /api/v1/snapshot, unlike how SnapshotR and OldSnashotBranchR work.
data ApiSnapshotName
    = ApiSnapshotName SnapName
    | ApiSnapshotNameBranch SnapshotBranch
    deriving (Eq, Show, Read)

instance PathPiece ApiSnapshotName where
    fromPathPiece x =
        ApiSnapshotName <$> fromPathPiece x
        <|> ApiSnapshotNameBranch <$> fromPathPiece x

    toPathPiece (ApiSnapshotName name) = toPathPiece name
    toPathPiece (ApiSnapshotNameBranch branch) = toPathPiece branch
