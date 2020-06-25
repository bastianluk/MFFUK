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

-- Closest future tournament
CREATE VIEW ClosestTournamentPerTeam AS
  SELECT
    t.Name AS [TeamName],
    closest.Name AS [TournamentName],
    closest.StartUtc as [StartUtc],
    closest.Location as [Location]
  FROM Team t
  INNER JOIN (
	SELECT TOP(1)
    tour.StartUtc AS [StartUtc],
    tour.[Location] AS [Location],
    p.TeamId AS [TeamId],
    tour.Name AS [Name]
    FROM TournamentParticipant p
    INNER JOIN Tournament tour on tour.Id = p.TournamentId
		GROUP BY p.TeamId
		ORDER BY tour.StartUtc ASC
	) closest
	on closest.TeamId = t.Id
GO

-- Team history (stats plus VS who)
CREATE VIEW TeamHistory AS
  SELECT 
    t.Name,
    s.Result,
    s.TournamentSeries
  FROM Team t
  INNER JOIN (
    SELECT
    case 
			when s.[SideATeamId] = t.Id then s.[SideBTeamId]
			when s.[SideBTeamId] = t.Id then s.[SideATeamId]
	  end as [OpponentId]
    from TournamentSeries s
    where Result IS NOT NULL
    ORDER BY StartUtc
  ) oppenent on --finish
GO

-- TODO
-- Player history (stats plus VS who)
CREATE VIEW PlayerHistory AS
GO

-- TODO
-- TOP10 Best teams per game (most wins in last 2 months)
CREATE VIEW Top10Teams AS
GO

-- TODO
-- TOP25 Best players in rated games
CREATE VIEW Top25Players AS
  SELECT TOP(25)
    p.Nickname,
    (SELECT AVG(Rating)
        FROM MatchPlayerStats s
		JOIN Match m on m.Id = s.MatchId 
        where m.StartUtc > DATEADD(month, -2, GETDATE())) as AvgRating
  FROM Player
  ORDER BY AvgRating DESC
GO

-- TOP10 Most active teams - most matches played.
CREATE VIEW Top10ActivePlayers AS
  SELECT TOP(10)
    t.Name,
    (SELECT Count(*)
      FROM Match m
      JOIN TournamentSeries s on s.Id = m.TournamentSeriesId
      where (
        m.StartUtc > DATEADD(month, -2, GETDATE()) AND (
          s.SideATeamId = t.Id OR
          s.SideBTeamId = t.Id
        )
      )
    ) as Count
  FROM Team t
  ORDER BY Count DESC
GO