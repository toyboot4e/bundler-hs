-- a pure re-export module; its own export chain is two levels deep
module Bulk (module Extra) where

import Extra
