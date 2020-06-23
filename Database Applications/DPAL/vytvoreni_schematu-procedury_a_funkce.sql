-- Adding new game with a valid format.
create procedure RegisterGame
	@Name nvarchar(32),
	@Format nvarchar(3)
as
	begin transaction;
		begin try
			-- Could be separate format table and just a simple if it can be selected from the table
			if ( not (@Format IN (select [Name] from MatchType)))
			begin
				RAISERROR('Invalid format value.', 11 , 1);				
			end

			-- Multiple formats for one game are supported
			insert into Game([Name], [DefaultFormat]) values (@Name, @Format);

		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go

--Creates a tournament series only if two teams are actual and two unique participants of the tournament
create procedure CreateTournamentSeries
	@TournamentId INT,
	@StartUtc DATE,
	@EndUtc DATE,
	@Stage NVARCHAR(32), -- Group stage, Lower/Upper bracket round, X round of playoffs, Finals
	@FormatBestOf INT, -- storing just the BoX value
	@SideATeamId INT,
	@SideBTeamId INT
as
	begin transaction;
		begin try
			-- Only 2 unique and actual participants of a tournament can compete.
			if ( not ( exists (
			    select * from TournamentParticipant where TournamentId = @TournamentId and TeamId = @SideATeamId
			) and exists (
				select * from TournamentParticipant where TournamentId = @TournamentId and TeamId = @SideBTeamId
			) and @SideATeamId != @SideBTeamId
			))
			begin
				RAISERROR('Teams have to be unique, existing and participants of the tournament.', 11 , 1);
			end
			
			-- Always at least one game has to be played.
			if (@FormatBestOf < 1)
			begin
				RAISERROR('BestOfX can only be positive integer', 11 , 1);
			end
			
			insert into TournamentSeries (TournamentId, StartUtc, EndUtc, Stage, FormatBestOf, SideATeamId, SideBTeamId)
				values (@TournamentId, @StartUtc, @EndUtc, @Stage, @FormatBestOf, @SideATeamId, @SideBTeamId)

		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go

--Create 1v1 Match
create procedure Create1v1Match
	@MatchTypeId INT,
	@TournamentSeriesId INT,
	@Result INT,
	@StartUtc DATE,
	@EndUtc DATE,
	@A1Id BIGINT,
	@B1Id BIGINT
as
	begin transaction;
		begin try
			
			declare select PlayingForTeamId from Player where Id = @A1id
		-- Check all players in org or contracted to play for team
			if ( not ( select PlayingForTeamId from Player where Id = @A1id))
			begin
				RAISERROR('Teams have to be unique and participants of the tournament.', 11 , 1);
			end
			
			
			insert into [Match] (MatchTypeId, TournamentSeriesId, Result, StartUtc, EndUtc, A1Id, B1Id)
				values (@MatchTypeId, @TournamentSeriesId, @Result, @StartUtc, @EndUtc, @A1Id, @B1Id);

		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go

CREATE FUNCTION PlaysForTeam (@PId BigInt, @TId Int)
RETURNS BIT
AS
BEGIN
    DECLARE @PlayingForTeamId Table;
    SET @PlayingForTeamId = select top(1) PlayingForTeamId from Player where Id = @A1id;
	DECLARE @Answer BIT;
    SET @ISOweek=1;
    RETURN(@ISOweek);
END;
GO

	select PlayingForTeamId from Player where Id = @A1id

--Create 5v5 Match
create procedure Create5v5Match
	@MatchTypeId INT,
	@TournamentSeriesId INT, 
	@Result INT,
	@StartUtc DATE, -- Handle "is actually played in the tournament span" via a trigger
	@EndUtc DATE,
	@A1Id BIGINT, 
	@A2Id BIGINT, 
	@A3Id BIGINT, 
	@A4Id BIGINT, 
	@A5Id BIGINT, 
	@B1Id BIGINT, 
	@B2Id BIGINT, 
	@B3Id BIGINT, 
	@B4Id BIGINT, 
	@B5Id BIGINT
as
	begin transaction;
		begin try
			declare @MatchTypeId INT;
			--select Id from [MatchType] where Name = '5v5'
			-- Check all players in org or contracted to play for team
			if ( not ( True ))
			begin
				RAISERROR('Teams have to be unique and participants of the tournament.', 11 , 1);
			end
			
			insert into [Match](MatchTypeId, TournamentSeriesId, Result, StartUtc, EndUtc, A1Id, A2Id, A3Id, A4Id, A5Id, B1Id, B2Id, B3Id, B4Id, B5Id)
				values (@MatchTypeId, @TournamentSeriesId, @Result, @StartUtc, @EndUtc, @A1Id, @A2Id, @A3Id, @A4Id, @A5Id, @B1Id, @B2Id, @B3Id, @B4Id, @B5Id);

		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go

-- TODO
--search matches by team


-- TODO
--Retire a player

-- TODO
--Transfer plyer

-- TODO
--Load player

-- TODO
--Player results (can be used by a view)

-- TODO
--Get player by name


--Schéma by mělo být rozumně indexované, aby dotazy na všechny data pohledů nevyžadovaly více než jeden FULL-SCAN (na řídící tabulku). SQL příkazy v procedurách a funkcích by kromě odůvodněných situací neměly vyžadovat žádný FULL-SCAN, protože se manipuluje s omezenou sadou řádek. 