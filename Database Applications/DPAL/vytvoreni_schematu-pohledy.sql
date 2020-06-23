-- Show all teams and their covering organizations grouped by game
CREATE VIEW TeamsByGames AS
  SELECT
    g.Name as [GameName],
    o.Name as [OrgName],
    t.Name as [TeamName]
  FROM Team t
  INNER JOIN Organization o on o.Id = t.OrganizationId
  INNER JOIN Game g on g.Id = t.GameId
  ORDER BY g.Name ASC, o.Name ASC
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
CREATE VIEW ClosestTournamentPerTeam AS
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


-- TODO
-- TOP10 Best teams per game (most wins in last 2 months)


-- TODO
-- TOP10 Best players in rated games
CREATE VIEW Top100Players AS
  SELECT TOP(100)
    p.Name,
    (SELECT AVG(
      CASE
        when m.[A1Id] = p.Id then m5.[A1Rating]
        when m.[A2Id] = p.Id then m5.[A2Rating]
        when m.[A3Id] = p.Id then m5.[A3Rating]
        when m.[A4Id] = p.Id then m5.[A4Rating]
        when m.[A5Id] = p.Id then m5.[A5Rating]
        when m.[B1Id] = p.Id then m5.[B1Rating]
        when m.[B2Id] = p.Id then m5.[B2Rating]
        when m.[B3Id] = p.Id then m5.[B3Rating]
        when m.[B4Id] = p.Id then m5.[B4Rating]
        when m.[B5Id] = p.Id then m5.[B5Rating]
      END
      )
        FROM Match m
        JOIN MatchStats m5 on m.Id = m5d
        where m.StartUtc > DATEADD(month, -2, GETDATE())) as Avg
  FROM Player
GO

-- TOP10 Most active teams - most matches played.
CREATE VIEW Top10Players AS
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