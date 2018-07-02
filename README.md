# Chess-backend

## Overview

This is the backend that powers the chess analytics web tool. Fundamentally, the goal of the backend is to provide a JSON API that's consumed by the frontend.

The backend consists of two parts:
- An application to parse external chess databases into the backend database
- A web server that provides API endpoints

## Main features

### Parsing external data

The external data for chess games comes in through a list of text files in Pgn format. A Pgn file simply describes the moves in a game. The main goal is to put this data into a SQL database is to allow a variety of queries. The philosophy is that this step should do as much of the hard work as possible, so that the resulting data is not corrupt and easy to query.

This library provides the `fill_db` application which takes those text files, parses them into games, and stores the relevant information in the database. The information in a Pgn file is unstructured. For instance, a player is referred to by their name "Carlsen, Magnus". The goal is to turn this into structured, queryable data, which includes the following:
- `Player` with a first and last name
- `Game`, which requires a player with the white and black pieces and a result. The `Game` includes a Pgn of the moves, and the moves are checked to parse into a game.
- A `MoveEval`, which provides an evaluation of every single position in the game. This data can be used to calculate how closely a player's moves match the recommendations by the computer.

### API endpoints

The API is provided by Servant. The beauty of Servant is that the API is fundamentally defined by it's (quite readable) type. The endpoints correspond to data pulls, for instance there is one data pull to obtain all the tournaments in the database, and another one to provide average evaluations by mover number.

When the resulting data pull is straightforward, for instance to get the list of tournaments, the API is a wrapper on top of a SQL query, which I try to do using Esqueleto. Other API endpoints, e.g. the move number evaluations, require more complicated data transformations that are done in Haskell proper. 

## Todo

The website (prod) not provide all the features that are available the development (dev) version.

The next big feature that rolls out will be user accounts, and the ability to upload own databases

### Additional statistics

- Summarize pawn structures
- Summarize playing style: How often do players exchange queens early, how often do they sacrifice?
- Show the win percentage based on evaluation. For instance, I would guess that, if you play someone who's rated 500 points higher than you, your win expectation will still be low even if your position is +2.
- Special window to prepare against opponent. This should show a player's openings and playing style

### Infrastructure

- Improve nginx caching
- Expand unit tests to cover all API endpoints and test user management features

## Technology

### Overview

- This backend is written in Haskell.  I use Snap to provide the web server, and Servant to provide the API inside the web server. For now, using Snap is overkill, but it will come in handy when allowing for user accounts and permissions.

- I use a postgres database, which I access using Persistent and Esqueleto.
- Everything is run on a single EC2 instance on AWS.

### Technical challenges and bottlenecks

Currently, the biggest challenge is that it takes a long time to read in the data and store evaluations. I find that one obtains reasonable evaluations with about 100ms of search time, but that means that parsing a database of, say, 1M games is prohibitive. I am considering running a cluster of EC2 instances to speed this up as needed.
