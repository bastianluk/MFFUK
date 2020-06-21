--Add Games
INSERT INTO Game([Name], [DefaultFormat])
VALUES
	('DOTA2', '5v5'),
	('CSGO', '5v5'),
	('HS', '1v1')

INSERT INTO Organization([Name], [OwnerName])
VALUES
	('G2', 'ocelot'), --CSGO HS
	('Team Liquid', 'uknown'), --CSGO DOTA HS
	('Alliance', 'admiralb'), --DOTA HS
	('OG Esports', 'seb'), --DOTA 2x
	('Refresh', 'zonic'), --CSGO 2x
	('Complexity', 'unknown') --DOTA HS

INSERT INTO Team([OrganizationId], [Name], [GameId])
VALUES
	(1,'G2 CSGO', 2),
	(1,'G2 HS', 3),
	(2,'TL DOTA', 1),
	(2,'TL CSGO', 2),
	(2,'TL HS', 3),
	(3,'Alliance', 1),
	(3,'Alliance HS', 3),
	(4,'OG Esports', 1),
	(4,'OG Seed', 1),
	(5,'Astralis', 2),
	(5,'Heroic', 2),
	(6,'Col Dota', 1),
	(6,'Col HS', 3)

INSERT INTO Player([Country], [Nickname], [ContractedOrgId], [PlayingForTeamId])
VALUES
	--G2 CSGO
	('France', 'Jackz', 1, 1),
	('France', 'kennys', 1, 1),
	('France', '', 1, 1),
	('Serbia', 'nexa', 1, 1),
	('Bosnia', 'hunter', 1, 1),
	--G2 HS
	('Romania', 'Rdu', 1, 2),
	('Romania', 'Thijs', 1, 2),
	--TL DOTA
	('Sweden', 'micke', 2, 3),
	('Sweden', 'boxi', 2, 3),
	('Sweden', 'insania', 2, 3),
	('Deutchland', 'qojqva', 2, 3),
	('Norway', 'taiga', 2, 3),
	--TL CSGO
	('USA', 'Stewie2k', 2, 4),
	('Canada', 'twistz', 2, 4),
	('Canada', 'NAF', 2, 4),
	('USA', 'elige', 2, 4),
	('USA', 'nitr0', 2, 4),
	--TL HS
	('USA', 'frozen', 2, 5),
	--Alliance
	('Sweden', 'Limmp', 3, 6),
	('Sweden', 'Handsken', 3, 6),
	('Sweden', 's4', 3, 6),
	('Belarus', 'fng', 3, 6),
	('Bulgaria', 'nikobaby', 3, 6),
	--Alliance HS
	('Sweden', 'orange', 3, 7),
	--OG Esports
	('Finland', 'topson', 4, 8),
	('France', 'seb', 4, 8),
	('Denmark', 'notail', 4, 8),
	('Malaysia', 'midone', 4, 8),
	('USA', 'sumail', 4, 8),
	--OG Seed
	('Greece', 'madara', 4, 9),
	('USA', 'chessie', 4, 9),
	('Sweden', 'xibbe', 4, 9),
	('Sweden', 'zfreak', 4, 9),
	('Finland', 'peksu', 4, 9),
	--Astralis
	('Denmark', 'device', 5, 10),
	('Denmark', 'xyp9x', 5, 10),
	('Denmark', 'dupreeh', 5, 10),
	('Denmark', 'magisk', 5, 10),
	('Denmark', 'glave', 5, 10),
	--Heroic
	('Denmark', 'cadian', 5, 11),
	('Denmark', 'niko', 5, 11),
	('Denmark', 'hunden', 5, 11),
	('Denmark', 'es3tag', 5, 11),
	('Denmark', 'snappi', 5, 11),
	--Col DOTA
	('Singapore', 'meracle', 6, 12),
	('Sweden', 'bananaslamjamma', 6, 12),
	('Brazil', 'tavo', 6, 12),
	('USA', 'Kyle', 6, 12),
	('Malaysia', 'adam', 6, 12),
	--Col HS
	('USA', 'Sottle', 6, 13)

-- TODO
INSERT INTO Tournament([Name], [Location], [Type], [StartUtc], [EndUtc], [GameId])
VALUES
	('The International 2019', 'Shanghai', 'Major', [StartUtc], [EndUtc], 1),
	('WePlay Winter', 'Ukraine', 'Minor', [StartUtc], [EndUtc], 1),
	('Boston Major', 'Boston', 'Major', [StartUtc], [EndUtc], 2),
	('HS GranPrix', 'LA', 'Major', [StartUtc], [EndUtc], 3)

INSERT INTO TournamentParticipant([TournamentId], [TeamId])
VALUES
	(1,3),
	(1,6),
	(1,8),
	(1,9),
	(2,3),
	(2,6),
	(2,9),
	(2,12),
	(3,1),
	(3,4),
	(3,10),
	(3,11),
	(4,2),
	(4,5),
	(4,7),
	(4,13)

-- TODO
INSERT INTO TournamentSeries([TournamentId], [StartUtc], [EndUtc], [Stage], [FormatBestOf], [State], [Result], [SideATeamId], [SideBTeamId])
VALUES
	(1, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(1, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(1, [StartUtc], [EndUtc], [Stage], 5, [State], [Result], [SideATeamId], [SideBTeamId]),
	(2, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(2, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(2, [StartUtc], [EndUtc], [Stage], 5, [State], [Result], [SideATeamId], [SideBTeamId]),
	(3, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(3, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(3, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(4, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(4, [StartUtc], [EndUtc], [Stage], 3, [State], [Result], [SideATeamId], [SideBTeamId]),
	(4, [StartUtc], [EndUtc], [Stage], 5, [State], [Result], [SideATeamId], [SideBTeamId])

INSERT INTO MatchType([Name])
VALUES
	('5v5'),
	('1v1'),
	('6v6')

-- TODO
INSERT INTO Match([MatchTypeId], [TournamentSeriesId], [Result], [StartUtc], [EndUtc], [A1Id], [A2Id], [A3Id], [A4Id], [A5Id], [A6Id], [B1Id], [B2Id], [B3Id], [B4Id], [B5Id], [B6Id])
VALUES
	(1, [TournamentSeriesId], [Result], [StartUtc], [EndUtc], [A1Id], [A2Id], [A3Id], [A4Id], [A5Id], [A6Id], [B1Id], [B2Id], [B3Id], [B4Id], [B5Id], [B6Id])

-- TODO
INSERT INTO MatchPlayerStats([MatchId], [PlayerId], [K], [D], [A], [Objective], [NetWorth], [Rating], [Class], [Deck])
VALUES
	(1, [PlayerId], [K], [D], [A], [Objective], [NetWorth], [Rating], [Class], [Deck])

-- TODO
INSERT INTO MatchStats([MatchTypeId], [A1StatsId], [A2StatsId], [A3StatsId], [A4StatsId], [A5StatsId], [A6StatsId], [B1StatsId], [B2StatsId], [B3StatsId], [B4StatsId], [B5StatsId], [B6StatsId])
VALUES
	(1, [A1StatsId], [A2StatsId], [A3StatsId], [A4StatsId], [A5StatsId], [A6StatsId], [B1StatsId], [B2StatsId], [B3StatsId], [B4StatsId], [B5StatsId], [B6StatsId])