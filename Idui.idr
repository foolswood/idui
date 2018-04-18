import Data.IORef

import VirtualDOM
import VirtualDOM.DOM

-- Text entry helpers

getString : Ptr -> JS_IO (Maybe String)
getString p = do
    ts <- jscall "typeof(%0)" (Ptr -> JS_IO String) p
    if ts == "string"
        then Just <$> jscall "%0" (Ptr -> JS_IO String) p
        else pure Nothing

dotLookup : Ptr -> String -> JS_IO Ptr
dotLookup = jscall "%0[%1]" (Ptr -> String -> JS_IO Ptr)

-- Stateful render wrapper

data RenderContext : Type where
  RenderState : (renderNode : Node) -> (currentlyRendered : IORef (Maybe Html)) -> RenderContext

newRenderContext : Node -> JS_IO RenderContext
newRenderContext node = newIORef' Nothing >>= pure . RenderState node

contextualRender : RenderContext -> Maybe Html -> JS_IO ()
contextualRender (RenderState renderNode currentlyRendered) nextHtml = do
    currentHtml <- readIORef' currentlyRendered
    render renderNode currentHtml nextHtml
    writeIORef' currentlyRendered nextHtml

-- Text box example

mutual
  redraw : (Maybe Html -> JS_IO ()) -> Ptr -> JS_IO ()
  redraw doRender p = do
    tp <- dotLookup p "target"
    vp <- dotLookup tp "value"
    mMsg <- getString vp
    let msg = maybe "shutup" id mMsg
    doRender $ Just $ html doRender msg

  html : (Maybe Html -> JS_IO ()) -> String -> Html
  html doRender msg =
    node "div" [] []
      [ node "input" [ on "input" (redraw doRender) noOptions ] [ ("value", msg), ("id", "ifield") ] []
      , node "label" [] [] [ text msg ]
      ]

main : JS_IO ()
main = do
  body <- documentBody
  rCtx <- newRenderContext body
  contextualRender rCtx $ Just $ html (contextualRender rCtx) "something"
