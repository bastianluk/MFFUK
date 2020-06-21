-- Internetový obchod s knihami

-- Kategorie jsou dvouurovnove, kazda kategorie je tedy bud hlavni, pak nema nadrazenou kategorii, nebo neni hlavni,
-- pak musi byt jeji nadrazena kategorie hlavni.
create TRIGGER Kategorie_Dve_Urovne ON Kategorie AFTER INSERT, UPDATE AS
  if (exists (select *
      from inserted
      left join Kategorie on inserted.IdNadrazenaKategorie = Kategorie.Id
      where Kategorie.IdNadrazenaKategorie IS NOT NULL)) -- nadøazená kategorie je už podkategorie
    OR (exists (select *
          from inserted
          where inserted.IdNadrazenaKategorie is not null
              and exists(select * from Kategorie where IdNadrazenaKategorie = inserted.Id))) -- kategorie má podkategorii i nadkategorii
  BEGIN
    ROLLBACK TRANSACTION;
    THROW 60000, 'Kategorie musi byt maximalne dvouurovnove.', 0;
  END;
GO

-- Pri odstraneni kategorie je nejprve nutne odstranit jeji podkategorie
CREATE TRIGGER Kategorie_Odstraneni_Podkategorii ON Kategorie INSTEAD OF DELETE AS
BEGIN
  -- odstraneni podkategorii
  delete from Kategorie where Kategorie.Id in (select K.Id from Kategorie K join deleted on Kategorie.IdNadrazenaKategorie = deleted.Id);

  -- odstraneni odstranovanych kategorii
  delete from Kategorie where Kategorie.Id in (select Id from deleted);
END

GO

-- Odstranìní knihy (vèetnì odstranìní seznamu jejích souvisejících knih a odstranìní knihy ze seznamu souvisejících knih jiných knih)
create trigger Kniha_Odstraneni On Kniha instead of delete as
begin
  delete from Souvisejici where Souvisejici.IdKniha in (select Id from deleted);
  delete from Souvisejici where Souvisejici.IdSouvisejiciKniha in (select Id from deleted);
  delete from Kniha where Kniha.Id in (select Id from deleted);
end
go