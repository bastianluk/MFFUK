-- Internetov� obchod s knihami

-- Kategorie jsou dvouurovnove, kazda kategorie je tedy bud hlavni, pak nema nadrazenou kategorii, nebo neni hlavni,
-- pak musi byt jeji nadrazena kategorie hlavni.
create TRIGGER Kategorie_Dve_Urovne ON Kategorie AFTER INSERT, UPDATE AS
  if (exists (select *
      from inserted
      left join Kategorie on inserted.IdNadrazenaKategorie = Kategorie.Id
      where Kategorie.IdNadrazenaKategorie IS NOT NULL)) -- nad�azen� kategorie je u� podkategorie
    OR (exists (select *
          from inserted
          where inserted.IdNadrazenaKategorie is not null
              and exists(select * from Kategorie where IdNadrazenaKategorie = inserted.Id))) -- kategorie m� podkategorii i nadkategorii
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

-- Odstran�n� knihy (v�etn� odstran�n� seznamu jej�ch souvisej�c�ch knih a odstran�n� knihy ze seznamu souvisej�c�ch knih jin�ch knih)
create trigger Kniha_Odstraneni On Kniha instead of delete as
begin
  delete from Souvisejici where Souvisejici.IdKniha in (select Id from deleted);
  delete from Souvisejici where Souvisejici.IdSouvisejiciKniha in (select Id from deleted);
  delete from Kniha where Kniha.Id in (select Id from deleted);
end
go