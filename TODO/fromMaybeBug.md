@ajnsit thanks, will try to take a look at your PR comments this weekend. Trying to get something up and running using some more basic features - a tabbed page selector. As part of that, I've got the following code: 

```purescript
    type Page = forall a. Widget HTML a

    pages :: Array Page
    pages = map (\tp -> tp.page) tPages
    emptyPage :: Page
    emptyPage = div' [text "No pages to show!"]
    pageAt :: Int -> Page
    pageAt ix = fromMaybe emptyPage (pages !! ix)
```

I don't see how there could be a mismatch in types but I get the error:

```
 src/Main.purs:166:17

  166      pageAt ix = fromMaybe emptyPage (pages !! ix)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  Could not match type
  
    a2
  
  with type
  
    a0
  
  while trying to match type Widget (Array ReactElement) t1
    with type Widget (Array ReactElement) a0
  while checking that expression (fromMaybe emptyPage) ((index pages) ix)
    has type Widget (Array ReactElement) a0
  in value declaration createTabWidget
  
  where a0 is a rigid type variable
          bound at (line 166, column 17 - line 166, column 50)
        a2 is a rigid type variable
          bound at (line 142, column 20 - line 142, column 21)
        t1 is an unknown type

```
