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


--Search matches of a team by its (partial) name
create function GetMatchesByTeam(
	@Name nvarchar(128)
) returns table
as
	return (
		select m.*
		from Match m
		join TournamentSeries t on t.Id = m.TournamentSeriesId 
		where (
			t.SideATeamId in (
				select Id
				from Team
				where [Name] like (@Name + '%')
			) OR
			t.SideBTeamId in (
				select Id
				from Team
				where [Name] like (@Name + '%')
			)
		)
	)
go

--Get player by name
create function GetPlayer(
	@Nickname nvarchar(128)
) returns table
as
	return (
		select *
		from Player
		where [Nickname] like (@Nickname + '%')
	)
go

--Search stats of a player by its (partial) nickname
create function GetPlayerStats(
	@Nickname nvarchar(128)
) returns table
as
	return (
		select *
		from MatchPlayerStats
		where PlayerId in (select Id from GetPlayer(@Nickname))
	)
go

--Player will play for a different team.
create procedure LoanPlayerTo
	@PId BIGINT,
	@TeamId INT
as
	begin transaction;
		begin try
			if ( exists (
			    select * from Player where PlayingForTeamId = @TeamId and Id = @PId
			) )
			begin
				RAISERROR('Player already on that team.', 11 , 1);
			end
						
			update Player
			set PlayingForTeamId = @TeamId
			where Id = @PId

		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go

--Players contracted will be moved to a different org.
create procedure TransferPlayerTo
	@PId BIGINT,
	@OrgId INT
as
	begin transaction;
		begin try
			if ( exists (
			    select * from Player where ContractedOrgId = @OrgId and Id = @PId
			) )
			begin
				RAISERROR('Player already contracted to that org.', 11 , 1);
			end
						
			update Player
			set ContractedOrgId = @OrgId
			where Id = @PId

		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go