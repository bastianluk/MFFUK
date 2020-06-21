-- Internetov� obchod s knihami

-- vytvo�� pr�zdnou objedn�vku ve stavu ko��ku (to je pot�eba pro nov�ho u�ivatele nebo pro u�ivatele, kter� sv�j ko��k potvrdil jako objedn�vku)
-- (pomocn� procedura pro proceduru UpravPocetVKosiku)
create procedure VytvorKosik
  @IdObjednavka bigint output -- vrac� Id ko��ku
as
  begin try
    set @IdObjednavka = CRYPT_GEN_RANDOM(7); -- pravd�podobnost neunik�tn� hodnoty je prakticky nulov�, m��eme si proto dovolit tento p��pad o�et�it jen vyhozen�m chyby
    insert into Objednavka (Id, Stav, Zaplacena) values (@IdObjednavka, 'Kosik', 0);
  end try
  begin catch
    throw 60001, 'Nepoda�ilo se vytvo�it novou objedn�vku (ko��k)', 0;
  end catch
go

-- pro danou knihu a ko��k p�id�/odebere po�et kus� z ko��ku
create procedure UpravPocetVKosiku
  @IdKniha int,
  @IdObjednavka bigint output, -- vstup - Id objednavky (ko��ku) nebo NULL (pak bude vytvo�ena nov� objedn�vka), vrac� id objedn�vky
  @Zmena int -- zm�na po�tu knih v ko��ku
as
  set transaction isolation level serializable;
  begin transaction;
    begin try
      -- kontrola jestli vypln�n� id skute�n� existuje
      if (@IdObjednavka IS NOT NULL)
      BEGIN
        if (not exists (select Id from Objednavka where Id = @IdObjednavka))
          throw 60002, 'Objedn�vka neexistuje', 0;
      END

      if (@IdObjednavka IS NULL)
        if (@Zmena <= 0)
          throw 60002, 'Objedn�vka neexistuje', 0;
        else
          execute VytvorKosik @IdObjednavka output;

      -- v tuto chvili je zarucena existence objednavky s @IdObjednavka

      declare @stav char(10);
      select @stav = Stav from Objednavka where Id = @IdObjednavka;
      if (@stav != 'Kosik')
        throw 60003, 'Objednavka neni ve stavu ''Kosik''.', 0;

      -- objednavka je nyni jiste ve stavu 'Kosik'

      declare @pocet int;
      select @pocet = Pocet from PolozkaObjednavky where IdObjednavka = @IdObjednavka and IdKniha = @IdKniha
      if @pocet is not NULL
      begin
        -- polozka s knihou existuje

        if (@pocet + @zmena != 0)
          -- pouze se uprav� hodnota u polo�ky
          update PolozkaObjednavky set Pocet = Pocet + @Zmena where IdKniha = @IdKniha and IdObjednavka = @IdObjednavka;
        else
          -- polo�ka se sma�e
          delete from PolozkaObjednavky where IdKniha = @IdKniha and IdObjednavka = @IdObjednavka;
      end
      else
      begin
        -- polozka s knihou neexistuje
        if (@Zmena <= 0)
          throw 60004, 'Polo�ka objedn�vky neexistuje', 0;
        else
          -- zalo�� se nov� polo�ka
          insert PolozkaObjednavky values (@IdObjednavka, @IdKniha, @Zmena);
      end
    end try

    begin catch
      rollback transaction;
      throw;
    end catch

  commit;
go

-- Z objedn�vky ve stavu ko��ku vytvo�� potvrzenou objedn�vku (na to jsou pot�eba �daje o kupuj�c�m), ze skladu ode�te knihy v objedn�vce
create procedure PotvrdObjednavku
  @IdObjednavka bigint,
  @IdTypDopravy int,
  @IdTypUhrady int,
  @Jmeno nvarchar(30),
  @Prijmeni nvarchar(30),
  @Ulice nvarchar(30),
  @Mesto nvarchar(30),
  @Psc char(5),
  @Email nvarchar(60)
as
  begin tran
    begin try
      declare @stav char(10);
      select @stav = Stav from Objednavka with (updlock) where Id = @IdObjednavka
      if (@stav != 'Kosik')
        throw 60005, 'Objedn�vka neexistuje nebo nen� ve stavu ''Kosik''', 0;

      if (not exists (select * from PolozkaObjednavky with (repeatableread) where IdObjednavka = @IdObjednavka))
        throw 60006, 'Objedn�vka nem� ��dn� polo�ky', 0;

      update Objednavka set Stav = 'Potvrzena',
                  IdTypDopravy = @IdTypDopravy,
                  IdTypUhrady = @IdTypUhrady,
                  Jmeno = @Jmeno,
                  Prijmeni = @Prijmeni,
                  Ulice = @Ulice,
                  Mesto = @Mesto,
                  Psc = @Psc,
                  Email = @Email,
                  CasPotvrzeni = CURRENT_TIMESTAMP
              where Id = @IdObjednavka;

      -- potvrzovat objedn�vku p�jde pouze pokud bude v�e dostupn� na skladu. Pokud ale mezi na�ten�m str�nky a potvrzen�m dojde ke sn�en� skladov�ch
      -- z�sob, neusp�je update kv�li IO a nastane rollback
      update Kniha set PocetNaSkladu = PocetNaSkladu - PolozkaObjednavky.Pocet
        from PolozkaObjednavky
        where Kniha.Id = PolozkaObjednavky.IdKniha and PolozkaObjednavky.IdObjednavka = @IdObjednavka
    end try
    begin catch
      rollback tran;
      throw;
    end catch
  commit;
go

-- stornuje objedn�vku a p�id� knihy z objedn�vky zp�t na sklad
create procedure StornujObjednavku
  @IdObjednavka bigint
as
  begin tran
    begin try
      update Objednavka set Stav = 'Stornovana'
        where Id = @IdObjednavka and Stav = 'Potvrzena';

      if @@ROWCOUNT = 0
        throw 60007, 'Objedn�vku se nepoda�ilo stornovat (pravd�podobn� neexistuje nebo nen� ve stavu ''Potvrzena'')', 0;

      update Kniha set PocetNaSkladu = PocetNaSkladu + PolozkaObjednavky.Pocet
          from PolozkaObjednavky
          where Kniha.Id = PolozkaObjednavky.IdKniha and PolozkaObjednavky.IdObjednavka = @IdObjednavka
    end try
    begin catch
      rollback;
      throw;
    end catch
  commit;
go

-- ozna�� objedn�vku jako zaplacenou, pokud je ve stavu potvrzen� a nen� je�t� zaplacen�
create procedure OznacObjednavkuZaplacena
  @IdObjednavka bigint
as
  begin try
    update Objednavka set Zaplacena = 1
      where Id = @IdObjednavka and Stav = 'Potvrzena' and Zaplacena = 0;

    if @@ROWCOUNT = 0
      throw 59999, 'Chyba', 0;
  end try
  begin catch
    throw 60008, 'Objedn�vku se nepoda�ilo ozna�it jako zaplacenou (pravd�podobn� neexistuje, nen� ve stavu ''Potvrzena'' nebo u� je zaplacen�)', 0;
  end catch
go

-- ozna�� objedn�vku jako nezaplacenou, pokud je ve stavu stornovan� a je zaplacen� (pou��v� se p�i vr�cen� pen�z z�kazn�kovi)
create procedure OznacObjednavkuNezaplacena
  @IdObjednavka bigint
as
  begin try
    update Objednavka set Zaplacena = 0
      where Id = @IdObjednavka and Stav = 'Stornovana' and Zaplacena = 1;

    if @@ROWCOUNT = 0
      throw 59999, 'Chyba', 0;
  end try
  begin catch
    throw 60009, 'Objedn�vku se nepoda�ilo ozna�it jako nezaplacenou (pravd�podobn� neexistuje, nen� ve stavu ''Stornovana'' nebo nebyla zaplacen�)', 0;
  end catch
go

-- pokud je objedn�vka potvrzen� a zaplacen�, tak ji ozna�� za odeslanou (pou�ije se v p��pad� odesl�n� knih z�kazn�kovi)
create procedure OznacObjednavkuOdeslana
  @IdObjednavka bigint
as
  begin try
    update Objednavka set Stav = 'Odeslana'
      where Id = @IdObjednavka and Stav = 'Potvrzena' and Zaplacena = 1;

    if @@ROWCOUNT = 0
      throw 59999, 'Chyba', 0;
  end try
  begin catch
    throw 60010, 'Objedn�vku se nepoda�ilo ozna�it jako odeslanou (pravd�podobn� neexistuje, nen� ve stavu ''Potvrzena'' nebo nebyla zaplacen�)', 0;
  end catch
go

-- u knihy aktualizuje po�et objednan�ch kus� u dodavatele
create procedure ObjednanoUDodavatele
  @IdKniha bigint,
  @Pocet int -- po�et objednan�ch knih (m��e b�t i z�porn�, pokud jsme nap�. u dodavatele objedn�vku stornovali)
as
  begin try
    update Kniha set PocetObjednano = PocetObjednano + @Pocet
      where Id = @IdKniha;

    if @@ROWCOUNT = 0
      throw 59999, 'Chyba', 0;
  end try
  begin catch
    throw 60011, 'Nepoda�ilo se zm�nit po�et objednan�ch knih', 0;
  end catch
go

-- u knihy p�esune ��st po�tu kus� objednan�ch u dodavatele do skladov�ch z�sob
create procedure PrisloOdDodavatele
  @IdKniha bigint,
  @Pocet int -- po�et knih, kter� p�i�el od dodavatele
as
  begin try
    update Kniha set PocetObjednano = PocetObjednano - @Pocet,
             PocetNaSkladu = PocetNaSkladu + @Pocet
      where Id = @IdKniha;

    if @@ROWCOUNT = 0
      throw 59999, 'Chyba', 0;
  end try
  begin catch
    throw 60012, 'Nepoda�ilo se knihy p�esunout z objednan�ch do skladov�ch', 0;
  end catch
go

-- spo�te celkovou cenu objedn�vky, tj. suma p�es v�echny polo�ky objedn�vky, kde se s��t� sou�in ceny knihy a po�tu kus�
create function CenaObjednavky(
  @IdObjednavka bigint
  )
returns numeric(10,2)
as
begin
  declare @celkem numeric(10,2);
  select @celkem = sum(PolozkaObjednavky.Pocet * Kniha.Cena)
    from Objednavka
      join PolozkaObjednavky on Objednavka.Id = PolozkaObjednavky.IdObjednavka
      join Kniha on PolozkaObjednavky.IdKniha = Kniha.Id
    where Objednavka.Id = @IdObjednavka;

  if @celkem is null
    set @celkem = 0;
  return @celkem;
end;
go