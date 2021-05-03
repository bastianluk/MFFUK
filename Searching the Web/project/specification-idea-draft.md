# Roster move tracker

by Lukas Bastian

## Idea

In any sport or esport it is pretty interesting to know who is going to play for who in the next season, year or generally any time period. It builds story lines, possibly points to friction in teams when a player requests to get traded or gets moved to the bench etc.

That is why we would like to create a scraper that would collect data about roster moves over our chosen domain, point to the source (articles, posts etc.) and possibly even categorize those moves into some general categories.

This could then be used to look for aditional storylines when it comes to the timing of moves (did this move A trigger another move B), you could later find out who made the announcement (was it a frustrated player or the organization).

Those are all interesting questions to us that the tool could help answer.

## Domain

Realistically this could be applied to any domain involving changes of position where it is very attractive for the public to know them, but think mostly sports or, in my case, esports.

It is a domain close to our hearts and we would like to pick the scene around a game called Counter-Strike: Global Offensive (CSGO for short) by Valve.

It was released in 2012 but in some capacity teams were switching to the game even prior to the official release but we don't think any valid data will be older than 2010.

## Data source

As far as we know there is no scientific data collection with such data that we would need so we thought as a part of this project we would have to select certain credible web pages as our sources and we would scrape those for information.

An approach we are not sure will lead to a good result is to scrape forums like reddit or hltv.org (a popular news website and forum, stories by journalists should be considered, but also a forum  posts by random people shall not be).

## Target

- Select or rather narrow down web pages that could be used as sources.
- Scrape the websites for content that mentions roster moves or new teams forming.
- Create a data structure (table should suffice) with the scrapped data.
  - Possibly categorize such content for easier search - e.g. into rumors, signed deals, ideas, player requests, by team etc.