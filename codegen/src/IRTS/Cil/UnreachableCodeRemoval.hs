{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module IRTS.Cil.UnreachableCodeRemoval
       (removeUnreachable) where

import Data.Graph
import Language.Cil hiding (entryPoint)

removeUnreachable :: [MethodDef] -> [MethodDef]
removeUnreachable defs = let (graph, fromVertex, toVertex) = callGraph defs
                             (Just entryPoint)             = toVertex "'runMain0'"
                         in map (def . fromVertex) (reachable graph entryPoint)
  where def (d, _, _) = d

callGraph :: [MethodDef]
          -> (Graph, Vertex -> (MethodDef, MethodName, [MethodName]), MethodName -> Maybe Vertex)
callGraph methods = graphFromEdges $ map toEdge methods
  where toEdge m@(Method _ _ name _ body)    = (m, name, callees body)
        callees (OpCode (Tailcall c) : rest) = callees (OpCode c : rest)
        callees (OpCode Call{..}     : rest) = methodName : callees rest
        callees (_                   : rest) = callees rest
        callees []                           = []
