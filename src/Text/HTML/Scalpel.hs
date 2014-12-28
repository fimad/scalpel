-- |
-- Scalpel is a web scraping library inspired by libraries like parsec and
-- Perl's <http://search.cpan.org/~miyagawa/Web-Scraper-0.38/ Web::Scraper>.
-- Scalpel builds on top of "Text.HTML.TagSoup" to provide a declarative and
-- monadic interface.
--
-- There are two general mechanisms provided by this library that are used to
-- build web scrapers: Selectors and Scrapers.
--
--
-- Selectors describe a location within an HTML DOM tree. The simplest selector,
-- that can be written is a simple string value. For example, the selector
-- @\"div\"@ matches every single div node in a DOM. Selectors can be combined
-- using tag combinators. The '//' operator to define nested relationships
-- within a DOM tree. For example, the selector @\"div\" \/\/ \"a\"@ matches all
-- anchor tags nested arbitrarily deep within a div tag.
--
-- In addition to describing the nested relationships between tags, selectors
-- can also include predicates on the attributes of a tag. The '@:' operator
-- creates a selector that matches a tag based on the name and various
-- conditions on the tag's attributes. An attribute predicate is just a function
-- that takes an attribute and returns a boolean indicating if the attribute
-- matches a criteria. There are several attribute operators that can be used
-- to generate common predicates. The '@=' operator creates a predicate that
-- matches the name and value of an attribute exactly. For example, the selector
-- @\"div\" \@: [\"id\" \@= \"article\"]@ matches div tags where the id
-- attribute is equal to @\"article\"@.
--
--
-- Scrapers are values that are parameterized over a selector and produce
-- a value from an HTML DOM tree. The 'Scraper' type takes two type parameters.
-- The first is the string like type that is used to store the text values
-- within a DOM tree. Any string like type supported by "Text.StringLike" is
-- valid. The second type is the type of value that the scraper produces.
--
-- There are several scraper primitives that take selectors and extract content
-- from the DOM. Each primitive defined by this library comes in two variants:
-- singular and plural. The singular variants extract the first instance
-- matching the given selector, while the plural variants match every instance.
--
--
-- The following is an example that demonstrates most of the features provided
-- by this library. Supposed you have the following hypothetical HTML located at
-- @\"http://example.com/article.html\"@ and you would like to extract a list of
-- all of the comments.
--
-- > <html>
-- >   <body>
-- >     <div class='comments'>
-- >       <div class='comment container'>
-- >         <span class='comment author'>Sally</span>
-- >         <div class='comment text'>Woo hoo!</div>
-- >       </div>
-- >       <div class='comment container'>
-- >         <span class='comment author'>Bill</span>
-- >         <img class='comment image' src='http://example.com/cat.gif' />
-- >       </div>\
-- >       <div class='comment container'>
-- >         <span class='comment author'>Susan</span>
-- >         <div class='comment text'>WTF!?!</div>
-- >       </div>
-- >     </div>
-- >   </body>
-- > </html>
--
-- The following snippet defines a function, @allComments@, that will download
-- the web page, and extract all of the comments into a list:
--
-- > type Author = String
-- >
-- > data Comment
-- >     = TextComment Author String
-- >     | ImageComment Author URL
-- >
-- > allComments :: IO (Maybe [Comments])
-- > allComments = scrapeURL "http://example.com/article.html" comments
-- >    where
-- >        comments :: Scraper String [Comment]
-- >        comments = chroots ("div" @: [hasClass "container"]) comment
-- >
-- >        comment :: Scraper String Comment
-- >        comment = textComment <|> imageComment
-- >
-- >        textComment :: Scraper String Comment
-- >        textComment = do
-- >            author      <- text $ "span" @: [hasClass "author"]
-- >            commentText <- text $ "div"  @: [hasClass "text"]
-- >            return $ TextComment author commentText
-- >
-- >        imageComment :: Scraper String Comment
-- >        imageComment = do
-- >            author   <- text       $ "span" @: [hasClass "author"]
-- >            imageURL <- attr "src" $ "img"  @: [hasClass "image"]
-- >            return $ ImageComment author imageURL
module Text.HTML.Scalpel (
-- * Selectors
    Selector
,   Selectable (..)
,   AttributePredicate
,   AttributeName
,   TagName
-- ** Wildcards
,   Any (..)
-- ** Tag combinators
,   (//)
-- ** Attribute predicates
,   (@:)
,   (@=)
,   (@=~)
,   hasClass
-- ** Executing selectors
,   select

-- * Scrapers
,   Scraper
-- ** Primitives
,   attr
,   attrs
,   text
,   texts
,   chroot
,   chroots
-- ** Executing scrapers
,   scrape
,   scrapeStringLike
,   URL
,   scrapeURL
) where

import Text.HTML.Scalpel.Internal.Scrape
import Text.HTML.Scalpel.Internal.Scrape.StringLike
import Text.HTML.Scalpel.Internal.Scrape.URL
import Text.HTML.Scalpel.Internal.Select
import Text.HTML.Scalpel.Internal.Select.Combinators
import Text.HTML.Scalpel.Internal.Select.Types
