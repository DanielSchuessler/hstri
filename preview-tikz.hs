import Tikz.Preview
import ReadArgs

main = do
    fn :& () <- readArgs
    previewTikzFile fn ((reverse . drop 4 . reverse) fn)
