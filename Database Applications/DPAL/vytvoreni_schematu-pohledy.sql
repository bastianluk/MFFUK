-- Show all teams and their covering organizations grouped by game
CREATE VIEW TeamsByGames AS
  SELECT
    g.Name as [GameName],
    t.Name as [TeamName],
    o.Name as [OrgName]
  FROM Team t
  INNER JOIN Organization o on o.Id = t.OrganizationId
  INNER JOIN Game g on g.Id = t.GameId
GO

-- Last tournament of a team
CREATE VIEW LastTournamentPerTeam AS
  SELECT
    t.Name AS [TeamName],
    tour.Name AS [TournamentName],
    MAX(tour.StartUtc) as [StartUtc],
    tour.Location as [Location]
  FROM Team t
  INNER JOIN TournamentParticipant p on p.TeamId = t.Id
  INNER JOIN Tournament tour on tour.Id = p.TournamentId
  WHERE tour.StartUtc < GETDATE()
  GROUP BY t.Name, tour.Name, tour.Location
GO

-- Player history (past matches, vs what opponent/team)
CREATE VIEW PlayerHistory AS
	select
		p.Nickname,
		p.Id as PlayerId,
		s.MatchId as MatchId,
		s.K,
		s.D,
		s.A,
		s.Objective,
		s.NetWorth,
		s.Rating,
		s.Class,
		s.Deck,
		case
			when p.PlayingForTeamId = series.SideATeamId then series.SideBTeamId
			when p.PlayingForTeamId = series.SideBTeamId then series.SideATeamId
		end as OpponentId
	from MatchPlayerStats s
	join Player p on p.Id = s.PlayerId
	join Match m on m.Id = s.MatchId
	join TournamentSeries series on series.Id = m.TournamentSeriesId
	where series.StartUtc < GETDATE()
GO

-- TOP10 Best teams per game (most wins in last 24 months)
CREATE VIEW Top10Teams AS
	SELECT * FROM (
	  SELECT TOP(10)
		t.Name,
		(SELECT COUNT(*)
			from Match m
			JOIN TournamentSeries series on series.Id = m.TournamentSeriesId
			where
				series.StartUtc > DATEADD(month, -24, GETDATE()) AND 
				(CASE
					WHEN t.Id = series.SideATeamId AND series.Result = 1 THEN 1
					WHEN t.Id = series.SideBTeamId AND series.Result = 2 THEN 1
					ELSE 0
				END) = 1
		) as Wins,
		t.Id as TeamId
	  FROM Team t
	  ORDER BY Wins DESC
	) result
	WHERE result.Wins IS NOT NULL
GO

-- TOP25 Best players (best avg rating in games in last 24 months)
CREATE VIEW Top25Players AS
	SELECT * FROM (
	  SELECT TOP(25)
		p.Nickname,
		(SELECT AVG(s.Rating)
			FROM TournamentSeries series
			JOIN Match m on m.TournamentSeriesId = series.Id
			JOIN MatchPlayerStats s on s.MatchId = m.Id
			where series.StartUtc > DATEADD(month, -24, GETDATE()) and s.PlayerId = p.Id and Rating IS NOT NULL
		) as AvgRating,
		p.Id as PlayerId
	  FROM Player p
	  ORDER BY AvgRating DESC
	) result
	WHERE result.AvgRating IS NOT NULL
GO

-- TOP20 Most active teams (most matches played in last 24 months)
CREATE VIEW Top20ActivePlayers AS
	SELECT * FROM (
	  SELECT TOP(20)
		p.Nickname,
		(SELECT COUNT(*)
			from Match m
			JOIN TournamentSeries series on series.Id = m.TournamentSeriesId
			where p.Id IN (m.A1Id, m.A2Id, m.A3Id, m.A4Id, m.A5Id, m.A6Id, m.B1Id, m.B2Id, m.B3Id, m.B4Id, m.B5Id, m.B6Id)
		) as GamesPlayed,
		g.Name as Game,
		p.Id as PlayerId
	  FROM Player p
	  JOIN Team t on t.Id = p.PlayingForTeamId
	  JOIN Game g on g.Id = t.GameId
	  ORDER BY GamesPlayed DESC
	) result
	WHERE result.GamesPlayed IS NOT NULL
GO

-- User friendly way of viewing tournament info.
CREATE VIEW TournamentParticipantIndex AS
	SELECT
		team.Name as Team,
		team.Id as Id,
		g.Name as [Game],
		t.Name as Tournament,
		t.Location as Locastion,
		t.StartUtc as [Start],
		case
			when t.EndUtc IS NOT NULL then t.EndUtc
			else 'Uknown or has not ended.'
		end as [END]
	FROM TournamentParticipant p
	INNER JOIN Tournament t on t.Id = p.TournamentId
	INNER JOIN Team team on team.Id = p.TeamId
	INNER JOIN Game g on g.Id = t.GameId
GO


CREATE VIEW MatchIndex AS
	select
		m.Id as MatchId,
		CONCAT('Bo', CAST(s.FormatBestOf AS varchar(3))) as BestOfX,
		m.Result as Result,
		s.SideATeamId as TeamAId,
		(select [Name] from Team where Id = s.SideATeamId) as TeamA,
		m.AScore as TeamAScore,
		m.BScore as TeamBScore,
		(select [Name] from Team where Id = s.SideBTeamId) as TeamB,
		s.SideBTeamId as TeamBId,
		s.Id as SeriesId
	from Match m
	inner join TournamentSeries s on s.Id = m.TournamentSeriesId
GO