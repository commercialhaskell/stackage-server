import Pantry (parseVersionThrowing, parsePackageNameThrowing)
import Pantry.Internal.Stackage (VersionP(VersionP), PackageNameP(PackageNameP))
import Stackage.Snapshot.Diff (SnapshotDiff, snapshotDiff, toDiffList)

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

snapshotDiffLength :: SnapshotDiff -> Int
snapshotDiffLength = length . toDiffList

main :: IO ()
main = do
  booleanName <- PackageNameP <$> parsePackageNameThrowing "Boolean"
  booleanVer <- VersionP <$> parseVersionThrowing "0.2.4"
  bookkeepingName <- PackageNameP <$> parsePackageNameThrowing "bookkeeping"
  bookkeepingVer <- VersionP <$> parseVersionThrowing "0.4.0.1"

  let
    boolean = (booleanName, booleanVer)
    bookkeeping = (bookkeepingName, bookkeepingVer)

  hspec $
    describe "Packages with upper case characters" $
      describe "Removal of bookkeeping package is one change" $ do
        it "[Boolean, bookkeeping] -> [Boolean]" $
          snapshotDiffLength (snapshotDiff [boolean, bookkeeping] [boolean]) `shouldBe` 1

        it "[bookkeeping, Boolean] -> [Boolean]" $
          snapshotDiffLength (snapshotDiff [bookkeeping, boolean] [boolean]) `shouldBe` 1
