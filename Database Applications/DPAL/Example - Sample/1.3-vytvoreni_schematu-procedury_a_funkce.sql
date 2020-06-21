-- Internetový obchod s knihami

-- vytvoøí prázdnou objednávku ve stavu košíku (to je potøeba pro nového uživatele nebo pro uživatele, který svùj košík potvrdil jako objednávku)
-- (pomocná procedura pro proceduru UpravPocetVKosiku)
create procedure VytvorKosik
  @IdObjednavka bigint output -- vrací Id košíku
as
  begin try
    set @IdObjednavka = CRYPT_GEN_RANDOM(7); -- pravdìpodobnost neunikátní hodnoty je prakticky nulová, mùžeme si proto dovolit tento pøípad ošetøit jen vyhozením chyby
    insert into Objednavka (Id, Stav, Zaplacena) values (@IdObjednavka, 'Kosik', 0);
  end try
  begin catch
    throw 60001, 'Nepodaøilo se vytvoøit novou objednávku (košík)', 0;
  end catch
go

-- pro danou knihu a košík pøidá/odebere poèet kusù z košíku
create procedure UpravPocetVKosiku
  @IdKniha int,
  @IdObjednavka bigint output, -- vstup - Id objednavky (košíku) nebo NULL (pak bude vytvoøena nová objednávka), vrací id objednávky
  @Zmena int -- zmìna poètu knih v košíku
as
  set transaction isolation level serializable;
  begin transaction;
    begin try
      -- kontrola jestli vyplnìné id skuteènì existuje
      if (@IdObjednavka IS NOT NULL)
      BEGIN
        if (not exists (select Id from Objednavka where Id = @IdObjednavka))
          throw 60002, 'Objednávka neexistuje', 0;
      END

      if (@IdObjednavka IS NULL)
        if (@Zmena <= 0)
          throw 60002, 'Objednávka neexistuje', 0;
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
          -- pouze se upraví hodnota u položky
          update PolozkaObjednavky set Pocet = Pocet + @Zmena where IdKniha = @IdKniha and IdObjednavka = @IdObjednavka;
        else
          -- položka se smaže
          delete from PolozkaObjednavky where IdKniha = @IdKniha and IdObjednavka = @IdObjednavka;
      end
      else
      begin
        -- polozka s knihou neexistuje
        if (@Zmena <= 0)
          throw 60004, 'Položka objednávky neexistuje', 0;
        else
          -- založí se nová položka
          insert PolozkaObjednavky values (@IdObjednavka, @IdKniha, @Zmena);
      end
    end try

    begin catch
      rollback transaction;
      throw;
    end catch

  commit;
go

-- Z objednávky ve stavu košíku vytvoøí potvrzenou objednávku (na to jsou potøeba údaje o kupujícím), ze skladu odeète knihy v objednávce
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
        throw 60005, 'Objednávka neexistuje nebo není ve stavu ''Kosik''', 0;

      if (not exists (select * from PolozkaObjednavky with (repeatableread) where IdObjednavka = @IdObjednavka))
        throw 60006, 'Objednávka nemá žádné položky', 0;

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

      -- potvrzovat objednávku pùjde pouze pokud bude vše dostupné na skladu. Pokud ale mezi naètením stránky a potvrzením dojde ke snížení skladových
      -- zásob, neuspìje update kvùli IO a nastane rollback
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

-- stornuje objednávku a pøidá knihy z objednávky zpìt na sklad
create procedure StornujObjednavku
  @IdObjednavka bigint
as
  begin tran
    begin try
      update Objednavka set Stav = 'Stornovana'
        where Id = @IdObjednavka and Stav = 'Potvrzena';

      if @@ROWCOUNT = 0
        throw 60007, 'Objednávku se nepodaøilo stornovat (pravdìpodobnì neexistuje nebo není ve stavu ''Potvrzena'')', 0;

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

-- oznaèí objednávku jako zaplacenou, pokud je ve stavu potvrzená a není ještì zaplacená
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
    throw 60008, 'Objednávku se nepodaøilo oznaèit jako zaplacenou (pravdìpodobnì neexistuje, není ve stavu ''Potvrzena'' nebo už je zaplacená)', 0;
  end catch
go

-- oznaèí objednávku jako nezaplacenou, pokud je ve stavu stornovaná a je zaplacená (používá se pøi vrácení penìz zákazníkovi)
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
    throw 60009, 'Objednávku se nepodaøilo oznaèit jako nezaplacenou (pravdìpodobnì neexistuje, není ve stavu ''Stornovana'' nebo nebyla zaplacená)', 0;
  end catch
go

-- pokud je objednávka potvrzená a zaplacená, tak ji oznaèí za odeslanou (použije se v pøípadì odeslání knih zákazníkovi)
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
    throw 60010, 'Objednávku se nepodaøilo oznaèit jako odeslanou (pravdìpodobnì neexistuje, není ve stavu ''Potvrzena'' nebo nebyla zaplacená)', 0;
  end catch
go

-- u knihy aktualizuje poèet objednaných kusù u dodavatele
create procedure ObjednanoUDodavatele
  @IdKniha bigint,
  @Pocet int -- poèet objednaných knih (mùže být i záporný, pokud jsme napø. u dodavatele objednávku stornovali)
as
  begin try
    update Kniha set PocetObjednano = PocetObjednano + @Pocet
      where Id = @IdKniha;

    if @@ROWCOUNT = 0
      throw 59999, 'Chyba', 0;
  end try
  begin catch
    throw 60011, 'Nepodaøilo se zmìnit poèet objednaných knih', 0;
  end catch
go

-- u knihy pøesune èást poètu kusù objednaných u dodavatele do skladových zásob
create procedure PrisloOdDodavatele
  @IdKniha bigint,
  @Pocet int -- poèet knih, který pøišel od dodavatele
as
  begin try
    update Kniha set PocetObjednano = PocetObjednano - @Pocet,
             PocetNaSkladu = PocetNaSkladu + @Pocet
      where Id = @IdKniha;

    if @@ROWCOUNT = 0
      throw 59999, 'Chyba', 0;
  end try
  begin catch
    throw 60012, 'Nepodaøilo se knihy pøesunout z objednaných do skladových', 0;
  end catch
go

-- spoète celkovou cenu objednávky, tj. suma pøes všechny položky objednávky, kde se sèítá souèin ceny knihy a poètu kusù
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