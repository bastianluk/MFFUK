INSERT INTO MatchType([Name])
VALUES
	('5v5'),
	('1v1'),
	('6v6')

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

--YYYY-MM-DD HH:MI:SS
INSERT INTO Tournament([Name], [Location], [Type], [StartUtc], [EndUtc], [GameId])
VALUES
	('The International 2019', 'Shanghai', 'Major', '2019-08-15 00:00:00', '2019-08-25 23:59:59', 1),
	('Boston Major', 'Boston', 'Major', '2018-01-12 00:00:00', '2019-01-28 23:59:59', 2),
	('2017 Hearthstone WC', 'Amsterdam', 'Major', '2018-01-18 00:00:00', '2018-01-21 23:59:59', 3)

INSERT INTO TournamentParticipant([TournamentId], [TeamId])
VALUES
	(1,3),
	(1,6),
	(1,8),
	(1,9),
	(2,1),
	(2,4),
	(2,10),
	(2,11),
	(3,2),
	(3,5),
	(3,7),
	(3,13)

INSERT INTO TournamentSeries([TournamentId], [StartUtc], [EndUtc], [Stage], [FormatBestOf], [Result], [SideATeamId], [SideBTeamId])
VALUES
	(1, '2019-08-24 20:59:59', '2019-08-24 23:59:59', 'Semifinals', 3, 1, 3, 6),
	(1, '2019-08-24 20:59:59', '2019-08-24 23:59:59', 'Semifinals', 5, 1, 3, 8),
	(1, '2019-08-25 20:59:59', '2019-08-25 23:59:59', 'Finals', 3, 1, 8, 9),
	(2, '2019-01-27 20:59:59', '2019-01-27 23:59:59', 'Semifinals', 3, 1, 1, 4),
	(2, '2019-01-27 20:59:59', '2019-01-27 23:59:59', 'Semifinals', 3, 1, 10, 11),
	(2, '2019-01-28 20:59:59', '2019-01-28 23:59:59', 'Finals', 5, 1, 1, 10),
	(3, '2018-01-20 20:59:59', '2018-01-20 23:59:59', 'Semifinals', 3, 1, 2, 5),
	(3, '2018-01-20 20:59:59', '2018-01-20 23:59:59', 'Semifinals', 3, 1, 7, 13),
	(3, '2018-01-21 20:59:59', '2018-01-21 23:59:59', 'Finals', 3, 1, 2, 7)

INSERT INTO MatchType([Name])
VALUES
	('5v5'),
	('1v1'),
	('6v6')

INSERT INTO Match([MatchTypeId], [TournamentSeriesId], [Result], [AScore], [BScore], [A1Id], [A2Id], [A3Id], [A4Id], [A5Id], [B1Id], [B2Id], [B3Id], [B4Id], [B5Id])
VALUES
	(1, 4, 1, 16, 10, 1, 2, 3, 4, 5, 13, 14, 15, 16, 17),
	(1, 4, 1, 16, 12, 1, 2, 3, 4, 5, 13, 14, 15, 16, 17),
	(1, 5, 1, 16, 13, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44),
	(1, 5, 1, 16, 5, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44),
	(1, 6, 1, 16, 5, 1, 2, 3, 4, 5, 35, 36, 37, 38, 39),
	(1, 6, 1, 16, 14, 1, 2, 3, 4, 5, 35, 36, 37, 38, 39)

INSERT INTO MatchPlayerStats([MatchId], [PlayerId], [K], [D], [A])
VALUES
	--finalsmatch1
	(1, 1, 28, 13, 2),
	(1, 2, 17, 13, 2),
	(1, 3, 20, 13, 2),
	(1, 4, 20, 13, 2),
	(1, 5, 14, 13, 2),
	(1, 35, 18, 16, 3),
	(1, 36, 7, 16, 3),
	(1, 37, 10, 16, 3),
	(1, 38, 10, 16, 3),
	(1, 39, 4, 16, 3),
	--finalsmatch2
	(1, 1, 23, 13, 2),
	(1, 2, 14, 13, 2),
	(1, 3, 23, 13, 2),
	(1, 4, 25, 13, 2),
	(1, 5, 11, 13, 2),
	(1, 35, 13, 16, 3),
	(1, 36, 2, 16, 3),
	(1, 37, 17, 16, 3),
	(1, 38, 12, 16, 3),
	(1, 39, 3, 16, 3)

--Needed if the match stats are being added independantly
SET IDENTITY_INSERT MatchStats ON;  
INSERT INTO MatchStats([Id], [MatchTypeId], [A1StatsId], [A2StatsId], [A3StatsId], [A4StatsId], [A5StatsId], [B1StatsId], [B2StatsId], [B3StatsId], [B4StatsId], [B5StatsId])
VALUES
	(5, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
	(6, 1, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
SET IDENTITY_INSERT MatchStats OFF;  
