module Bench.Triangulations where
import Triangulation
import Data.Binary
import qualified Data.ByteString.Lazy.Char8

-- | or_1.28448530
tr_0 :: LabelledTriangulation
tr_0 = ("or_1.28448530", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\r\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\ETX\STX"))

-- | or_1.46377664
tr_1 :: LabelledTriangulation
tr_1 = ("or_1.46377664", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NUL\CAN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\ETX\NUL"))

-- | or_1.52947733
tr_2 :: LabelledTriangulation
tr_2 = ("or_1.52947733", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\EOT"))

-- | or_1.58864664 (b)
tr_3 :: LabelledTriangulation
tr_3 = ("or_1.58864664 (b)", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\STX"))

-- | or_1.64960972
tr_4 :: LabelledTriangulation
tr_4 = ("or_1.64960972", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\NUL"))

-- | or_1.83193119 (a)
tr_5 :: LabelledTriangulation
tr_5 = ("or_1.83193119 (a)", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\ETX"))

-- | or_1.83193119 (c)
tr_6 :: LabelledTriangulation
tr_6 = ("or_1.83193119 (c)", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\r\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\ETX\NUL"))

-- | or_1.83193119 (d)
tr_7 :: LabelledTriangulation
tr_7 = ("or_1.83193119 (d)", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\NUL"))

-- | or_1.83193119 (e)
tr_8 :: LabelledTriangulation
tr_8 = ("or_1.83193119 (e)", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\SOH"))

-- | or_1.84358597 (a)
tr_9 :: LabelledTriangulation
tr_9 = ("or_1.84358597 (a)", decode (Data.ByteString.Lazy.Char8.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ETX\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\SOH\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ETX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\NUL"))

trs :: [LabelledTriangulation]
trs =
    [ tr_0
    , tr_1
    , tr_2
    , tr_3
    , tr_4
    , tr_5
    , tr_6
    , tr_7
    , tr_8
    , tr_9
    ]

-- vim: nowrap