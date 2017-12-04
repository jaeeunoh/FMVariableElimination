module Util where

import Lib
import Parser

fromRight (Right x) = x

-- util for testing:
prs = normalize . fromRight . parseEquation