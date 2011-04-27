module Version(package, version, fullName) where

import qualified Paths_ginsu as Paths
import Data.Version

package :: String
package = "ginsu"

version :: String
version = showVersion Paths.version

fullName :: String
fullName = package ++ '-' : version
