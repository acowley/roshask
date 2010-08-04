{-# LANGUAGE PackageImports #-}
-- |Tuples in Haskell can be represented as heterogenous arrays in the
-- XML-RPC type system. This module defines instances for tuples up to
-- length 5.
module Ros.Util.XmlRpcTuples where
import "mtl" Control.Monad.Error
import Network.XmlRpc.Internals

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c, XmlRpcType d, 
          XmlRpcType e) => 
         XmlRpcType (a,b,c,d,e) where
    toValue (v,w,x,y,z) = 
        ValueArray [toValue v, toValue w, toValue x, toValue y, toValue z]
    fromValue (ValueArray [v,w,x,y,z]) = 
        liftM5 (,,,,) (fromValue v) (fromValue w) (fromValue x) 
                      (fromValue y) (fromValue z) 
    fromValue _ = throwError "Expected 5-element tuple!"
    getType _ = TArray

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c, XmlRpcType d) => 
         XmlRpcType (a,b,c,d) where
    toValue (w,x,y,z) = ValueArray [toValue w, toValue x, toValue y, toValue z]
    fromValue (ValueArray [w,x,y,z]) = 
        liftM4 (,,,) (fromValue w) (fromValue x) (fromValue y) (fromValue z)
    fromValue _ = throwError "Expected 4-element tuple!"
    getType _ = TArray

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c) => XmlRpcType (a,b,c) where
    toValue (x,y,z) = ValueArray [toValue x, toValue y, toValue z]
    fromValue (ValueArray [x,y,z]) = 
        liftM3 (,,) (fromValue x) (fromValue y) (fromValue z)
    fromValue _ = throwError "Expected 3-element tuple!"
    getType _ = TArray

instance (XmlRpcType a, XmlRpcType b) => XmlRpcType (a,b) where
    toValue (x,y) = ValueArray [toValue x, toValue y]
    fromValue (ValueArray [x,y]) = liftM2 (,) (fromValue x) (fromValue y)
    fromValue _ = throwError "Expected 2-element tuple."
    getType _ = TArray
