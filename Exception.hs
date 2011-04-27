{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,0,0)
#define MODULE Control.OldException
#else
#define MODULE Control.Exception
#endif
module Exception (module MODULE) where import MODULE
