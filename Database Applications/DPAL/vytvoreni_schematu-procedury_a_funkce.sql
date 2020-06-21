-- Adding new game with a valid format.
create procedure RegisterGame
	@Name nvarchar(32) NOT NULL,
	@Format nvarchar(3) NOT NULL
as
	begin transaction;
		begin try
			-- Could be separate format table and just a simple if it can be selected from the table
			if ( not (@Format IN ('5v5', '1v1')))
			begin
				raiseerror('Invalid format value.', 10 , 1);				
			end

			-- Multiple formats for one game are supported
			insert into Game (Name, Format) values (@Name, @Format);

		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go


--Creates a tournament series only if two teams are actual and two unique participants of the tournament
create procedure CreateTournamentSeries
	@TournamentId INT NOT NULL,
	@StartUtc DATE NOT NULL,
	@EndUtc DATE,
	@Stage NVARCHAR(32) NOT NULL, -- Group stage, Lower/Upper bracket round, X round of playoffs, Finals
	@FormatBestOf INT NOT NULL, -- storing just the BoX value
	@SideATeamId INT NOT NULL,
	@SideBTeamId INT NOT NULL
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
				raiseerror('Teams have to be unique and participants of the tournament.', 10 , 1);
			end
			
			-- Always at least one game has to be played.
			if (@FormatBestOf < 1)
			begin
				raiseerror('BestOfX can only be positive integer', 10 , 1);
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
	@StartUtc DATE NOT NULL, -- Handle "is actually played in the tournament span" via a trigger
	@EndUtc DATE,
	@A1Id BIGINT,
	@B1Id BIGINT
as
	begin transaction;
		begin try

		-- Check all players in org or contracted to play for team
			if ( not ( True ))
			begin
				raiseerror('Teams have to be unique and participants of the tournament.', 10 , 1);
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

--Create 5v5 Match

create procedure Create5v5Match
	@MatchTypeId INT NOT NULL,
	@TournamentSeriesId INT NOT NULL, 
	@Result INT,
	@StartUtc DATE NOT NULL, -- Handle "is actually played in the tournament span" via a trigger
	@EndUtc DATE,
	@A1Id BIGINT NOT NULL, 
	@A2Id BIGINT NOT NULL, 
	@A3Id BIGINT NOT NULL, 
	@A4Id BIGINT NOT NULL, 
	@A5Id BIGINT NOT NULL, 
	@B1Id BIGINT NOT NULL, 
	@B2Id BIGINT NOT NULL, 
	@B3Id BIGINT NOT NULL, 
	@B4Id BIGINT NOT NULL, 
	@B5Id BIGINT NOT NULL
as
	begin transaction;
		begin try
			declare @MatchTypeId INT;
			--select Id from [MatchType] where Name = '5v5'
			-- Check all players in org or contracted to play for team
			if ( not ( True ))
			begin
				raiseerror('Teams have to be unique and participants of the tournament.', 10 , 1);
			end
			
			insert into [Match] (MatchTypeId, TournamentSeriesId, Result, StartUtc, EndUtc, A1Id, A2Id, A3Id, A4Id, A5Id, B1Id, B2Id, B3Id, B4Id, B5Id)
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
--MarkLastMatch of TeamId as won

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