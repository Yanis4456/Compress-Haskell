import Test.HUnit
import qualified Statistic.Huffman as Huffman
import qualified Statistic.ShannonFano as ShF
import Statistic.EncodingTree (compress, uncompress, meanLength)
import Data.Maybe (fromJust)

testHuffman :: Test
testHuffman = TestCase (assertEqual "Compress and Uncompress abbac" (Just "abbca") (uncompress (compress Huffman.tree "abbca")))

testShF :: Test
testShF = TestCase (assertEqual "Compress and Uncompress abbac" (Just "abbca") (uncompress (compress ShF.tree "abbca")))

emptyHuffman :: Test
emptyHuffman = TestCase (assertEqual "Compress and Uncompress abbac" Nothing (uncompress (compress Huffman.tree "")))

emptyShf :: Test
emptyShf = TestCase (assertEqual "Compress and Uncompress abbac" Nothing (uncompress (compress ShF.tree "")))

oneHuffman :: Test
oneHuffman = TestCase (assertEqual "Compress and Uncompress abbac" (Just "a") (uncompress (compress Huffman.tree "a")))

oneShf :: Test
oneShf = TestCase (assertEqual "Compress and Uncompress abbac" (Just "a") (uncompress (compress ShF.tree "a")))

testShF_Huffman :: Test
testShF_Huffman = TestCase (assertEqual "Checking Mean Lenghts if equal" (meanLength (fromJust(Huffman.tree "abbca"))) (meanLength (fromJust(ShF.tree "abbca"))) )

tests :: Test
tests = TestList [TestLabel "Huffman" testHuffman, TestLabel "ShannonFano" testShF, TestLabel "Mean Length" testShF_Huffman,
                  TestLabel "empty ShannonFano" emptyShf, TestLabel "empty Huffman" emptyHuffman,
                  TestLabel "one Character ShanonFano" oneShf, TestLabel "one Character Huffman" oneHuffman]

main :: IO Counts
main = runTestTT tests
