-- Procedur and function tests
------------------------------

-- Cannot Register invalid format game
begin try
  execute RegisterGame 'Dead by Daylight', '1v4'
  print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- Can register a valid format game
begin try
  execute RegisterGame 'Quake Tournament', '1v1'
  print 'Test succeeded'
end try
begin catch
	print 'Test failed'
end catch

-- Cannot create a series of team A VS team A
declare @TournamentId INT;
select top(1) @TournamentId = t.Id from Tournament t inner join Game g ON g.Id = t.GameId where g.Name = 'CSGO'
declare @TeamAId BIGINT;
select top(1) @TeamAId = p.Id from TournamentParticipant p where p.TournamentId = @TournamentId

begin try
  execute CreateTournamentSeries @TournamentId, NULL, NULL, NULL, NULL, @TeamAId, @TeamAId
  print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- Get matches by partial team name (G2 - G)
select * from GetMatchesByTeam('G')
-- Get players by partial nickname
select * from GetPlayer('k')
-- Get playerstats by partial nickname
select * from GetPlayerStats('k')

-- Cannot loan to same team
declare @PlayerId INT;
select top(1) @PlayerId = p.Id from Player p inner join Team t on t.Id = p.ContractedOrgId inner join Game g ON g.Id = t.GameId where g.Name = 'CSGO'
declare @TeamId BIGINT;
select top(1) @TeamId = p.PlayingForTeamId from Player p where p.Id = @PlayerId;

begin try
  execute LoanPlayerTo @PlayerId, @TeamId
  print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- Cannot loan to same team
declare @Player2Id INT;
select top(1) @Player2Id = p.Id from Player p inner join Team t on t.Id = p.ContractedOrgId inner join Game g ON g.Id = t.GameId where g.Name = 'HS'
declare @PlayerOrgId BIGINT;
select top(1) @PlayerOrgId = p.ContractedOrgId from Player p where p.Id = @Player2Id;
declare @OrgId BIGINT;
select top(1) @OrgId = o.Id from Organization o where o.Id != @PlayerOrgId;

begin try
  execute TransferPlayerTo @Player2Id, @OrgId
  print 'Test succeeded'
end try
begin catch
	print 'Test failed'
end catch


-- View tests
-------------

-- Show all teams and their covering organizations grouped by game
select * from TeamsByGames

-- Last tournament of a team
select * from LastTournamentPerTeam

-- Player history (past matches, vs what opponent/team)
select * from PlayerHistory

-- TOP10 Best teams per game (most wins in last 24 months)
select * from Top10Teams

-- TOP25 Best players (best avg rating in games in last 24 months)
select * from Top25Players

-- TOP20 Most active teams (most matches played in last 24 months)
select * from Top20ActivePlayers

-- Tournament Index - get only participants of HS tournaments
select * from TournamentParticipantIndex where Game = 'HS'

-- MatchIndex - human friendly way of viewing matches
select * from MatchIndex