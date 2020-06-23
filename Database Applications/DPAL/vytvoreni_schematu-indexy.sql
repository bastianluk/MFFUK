-- FK Indexes

CREATE INDEX Team__OrganizationId ON Team(OrganizationId);
CREATE INDEX Team__GameId ON Team(GameId);
CREATE INDEX Player_ContractedOrgId On Player(ContractedOrgId);
CREATE INDEX Player_PlayingForTeamId On Player(PlayingForTeamId);
CREATE INDEX Tournament_GameId On Tournament(GameId);
CREATE INDEX TournamentParticipant_TournamentId ON TournamentParticipant(TournamentId);
CREATE INDEX TournamentParticipant_TeamId ON TournamentParticipant(TeamId);
CREATE INDEX TournamentSeries_TournamentId ON TournamentSeries(TournamentId);
CREATE INDEX TournamentSeries_SideATeamId ON TournamentSeries(SideATeamId);
CREATE INDEX TournamentSeries_SideBTeamId ON TournamentSeries(SideBTeamId);
CREATE INDEX Match_TournamentSeriesId On Match(TournamentSeriesId);
CREATE INDEX Match_MatchTypeId On Match(MatchTypeId);
CREATE INDEX Match_A1Id On Match(A1Id);
CREATE INDEX Match_A2Id On Match(A2Id);
CREATE INDEX Match_A3Id On Match(A3Id);
CREATE INDEX Match_A4Id On Match(A4Id);
CREATE INDEX Match_A5Id On Match(A5Id);
CREATE INDEX Match_A6Id On Match(A6Id);
CREATE INDEX Match_B1Id On Match(B1Id);
CREATE INDEX Match_B2Id On Match(B2Id);
CREATE INDEX Match_B3Id On Match(B3Id);
CREATE INDEX Match_B4Id On Match(B4Id);
CREATE INDEX Match_B5Id On Match(B5Id);
CREATE INDEX Match_B6Id On Match(B6Id);
CREATE INDEX MatchPlayerStats_MatchId On MatchPlayerStats(MatchId);
CREATE INDEX MatchPlayerStats_PlayerId On MatchPlayerStats(PlayerId);
CREATE INDEX MatchStats_Id_MatchTypeId On MatchStats(Id, MatchTypeId);
CREATE INDEX MatchStats_A1StatsId On MatchStats(A1StatsId);
CREATE INDEX MatchStats_A2StatsId On MatchStats(A2StatsId);
CREATE INDEX MatchStats_A3StatsId On MatchStats(A3StatsId);
CREATE INDEX MatchStats_A4StatsId On MatchStats(A4StatsId);
CREATE INDEX MatchStats_A5StatsId On MatchStats(A5StatsId);
CREATE INDEX MatchStats_A6StatsId On MatchStats(A6StatsId);
CREATE INDEX MatchStats_B1StatsId On MatchStats(B1StatsId);
CREATE INDEX MatchStats_B2StatsId On MatchStats(B2StatsId);
CREATE INDEX MatchStats_B3StatsId On MatchStats(B3StatsId);
CREATE INDEX MatchStats_B4StatsId On MatchStats(B4StatsId);
CREATE INDEX MatchStats_B5StatsId On MatchStats(B5StatsId);
CREATE INDEX MatchStats_B6StatsId On MatchStats(B6StatsId);


-- Specific indexes

CREATE INDEX Player_Nickname ON Player(Nickname);
CREATE INDEX Team_Name ON Team(Name);