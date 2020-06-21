-- Esports database

-- TODO
-- Specification and table creation

/*

---

For administrators
------------------


For users
---------


*/

CREATE TABLE Game (
	[Id] INT IDENTITY(1, 1)
		PRIMARY KEY,
	[Name] NVARCHAR(32) NOT NULL,
	[DefaultFormat] NVARCHAR(3) NOT NULL,
)

CREATE TABLE Organization (
	[Id] INT IDENTITY(1, 1)
		PRIMARY KEY,
	[Name] NVARCHAR(32) NOT NULL,
	[OwnerName] NVARCHAR(32) NOT NULL,
)

CREATE TABLE Team (
	[Id] INT IDENTITY(1, 1)
		PRIMARY KEY,
	[OrganizationId] INT
		FOREIGN KEY REFERENCES [Organization]([Id]),
	[Name] NVARCHAR(32) NULL,
	[GameId] INT
		FOREIGN KEY REFERENCES [Game]([Id]),
)

CREATE TABLE Player (
	[Id] BIGINT IDENTITY(1, 1)
		PRIMARY KEY,
	--[Name] NVARCHAR(32) NOT NULL,
	[Country] NVARCHAR(32) NULL,
	[Nickname] NVARCHAR(32) NULL,
	[ContractedOrgId] INT NOT NULL
		FOREIGN KEY REFERENCES [Organization]([Id]),
	[PlayingForTeamId] INT NOT NULL
		FOREIGN KEY REFERENCES [Team]([Id]),
	[IsActive] BIT NOT NULL DEFAULT 1,
)

CREATE TABLE Tournament (
	[Id] INT IDENTITY(1, 1)
		PRIMARY KEY,
	[Name] NVARCHAR(32) NOT NULL,
	[Location] NVARCHAR(32) NOT NULL,
	[Type] NVARCHAR(32) NOT NULL, -- Major, Minor, Online, Qualifier
	[StartUtc] DATE NOT NULL,
	[EndUtc] DATE,
	[GameId] INT
		FOREIGN KEY REFERENCES [Game]([Id]),
)

CREATE TABLE TournamentParticipant (
	[Id] BIGINT IDENTITY(1, 1)
		PRIMARY KEY,
	[TournamentId] INT
		FOREIGN KEY REFERENCES [Tournament]([Id]),
	[TeamId] INT
		FOREIGN KEY REFERENCES [Team]([Id]),
    CONSTRAINT TournamentParticipant_Tournament_Team UNIQUE ([TournamentId], [TeamId])
)

CREATE TABLE TournamentSeries (
	[Id] INT IDENTITY(1, 1)
		PRIMARY KEY,
	[TournamentId] INT
		FOREIGN KEY REFERENCES [Tournament]([Id]),
	[StartUtc] DATE NOT NULL,
	[EndUtc] DATE,
	[Stage] NVARCHAR(32) NOT NULL, -- Groups, Lower/Upper bracket, Playoffs
	[FormatBestOf] INT NOT NULL DEFAULT 1, -- storing just the BoX value
	[State] INT NOT NULL DEFAULT 0,
	[Result] INT NULL DEFAULT 0,
	[SideATeamId] INT
		FOREIGN KEY REFERENCES [Team]([Id]), -- Handle "is one of the participants" via a trigger
	[SideBTeamId] INT
		FOREIGN KEY REFERENCES [Team]([Id]), -- Handle "is one of the participants" via a trigger
)

CREATE TABLE [MatchType] (
	[Id] INT IDENTITY(1, 1)
		PRIMARY KEY,
	[Name] NVARCHAR(3), -- 5v5, 1v1, 6v6
)

CREATE TABLE [Match] (
	[Id] BIGINT IDENTITY(1, 1)
		PRIMARY KEY,
	[MatchTypeId] INT
		FOREIGN KEY REFERENCES [MatchType]([Id]), 
		CONSTRAINT Match_AltPK UNIQUE ([Id],[MatchTypeId]),
	[TournamentSeriesId] INT
		FOREIGN KEY REFERENCES [TournamentSeries]([Id]),
	[Result] INT DEFAULT 0,
	[StartUtc] DATE NOT NULL, -- Handle "is actually played in the tournament span" via a trigger
	[EndUtc] DATE,
	[A1Id] BIGINT -- SideA player 1
		FOREIGN KEY REFERENCES [Player]([Id]),
	[A2Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[A3Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[A4Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[A5Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[A6Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[B1Id] BIGINT -- SideB player 1
		FOREIGN KEY REFERENCES [Player]([Id]),
	[B2Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[B3Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[B4Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[B5Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id]),
	[B6Id] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [Player]([Id])
)

CREATE TABLE [MatchPlayerStats] (
	[Id] BIGINT IDENTITY(1, 1)
		PRIMARY KEY,
	[MatchId] BIGINT
		FOREIGN KEY REFERENCES [Match]([Id]),
	[PlayerId] BIGINT
		FOREIGN KEY REFERENCES [Player]([Id]),
	[K]INT NULL DEFAULT NULL,
	[D]INT NULL DEFAULT NULL,
	[A]INT NULL DEFAULT NULL,
	[Objective] INT NULL DEFAULT NULL,
	[NetWorth] INT NULL DEFAULT NULL,
	[Rating] INT NULL DEFAULT NULL,
	[Class] NVARCHAR(32) NULL DEFAULT NULL,
	[Deck] NVARCHAR(32) NULL DEFAULT NULL,
)

CREATE TABLE [MatchStats] (
	[Id] BIGINT IDENTITY(1, 1)
		PRIMARY KEY,
	[MatchTypeId] INT,
	FOREIGN KEY ([Id], [MatchTypeId]) REFERENCES [Match]([Id], [MatchTypeId]),
	[A1StatsId] BIGINT -- SideA player 1
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[A2StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[A3StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[A4StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[A5StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[A6StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[B1StatsId] BIGINT -- SideB player 1
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[B2StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[B3StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[B4StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[B5StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id]),
	[B6StatsId] BIGINT NULL DEFAULT NULL
		FOREIGN KEY REFERENCES [MatchPlayerStats]([Id])
)