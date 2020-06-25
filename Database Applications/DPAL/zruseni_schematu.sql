-- Drop Views
DROP VIEW TeamsByGames;
DROP VIEW LastTournamentPerTeam;
DROP VIEW PlayerHistory;
DROP VIEW Top10Teams;
DROP VIEW Top25Players;
DROP VIEW Top20ActivePlayers;

-- Drop Procedures
DROP PROCEDURE RegisterGame;
DROP PROCEDURE CreateTournamentSeries;
DROP PROCEDURE LoanPlayerTo;
DROP PROCEDURE TransferPlayerTo;

-- Drop Functions
DROP FUNCTION GetMatchesByTeam;
DROP FUNCTION GetPlayer;
DROP FUNCTION GetPlayerStats;

-- Drop Tables
DROP TABLE [MatchStats];
DROP TABLE [MatchPlayerStats];
DROP TABLE [Match];
DROP TABLE [MatchType];
DROP TABLE [TournamentSeries];
DROP TABLE [TournamentParticipant];
DROP TABLE [Tournament];
DROP TABLE [Player];
DROP TABLE [Team];
DROP TABLE [Organization];
DROP TABLE [Game];	