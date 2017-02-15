## Innan du börjar
### Pull
Innan du börjar arbeta är det viktigt att du drar hem de senaste ändringarna från vår repo. Det gör du genom att skriva följande i terminalen när du är i mappen:

```bash
$ git pull
```

## Efter ändringar

### status
När du är redo att "committa", är det bra vana att först kolla vad som har ändrats. Skriv följande i terminalen:

```bash
$ git status

On branch master
Your branch is up-to-date with 'origin/master'.
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	modified:   src/Tile.hs

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	ChangeLog.md
	LICENSE.save0
	Setup.hs
	diary.txt
	gitguide.md
	old.hs
```

Detta säger vilken branch du befinner dig i, och vilka filer som är ändrade (modified) och vilka som ännu inte lagts till (untracked).

Filer som är untracked kommer inte att laddas upp förrän du lägger till dem. Det gör du genom kommandot ``git add folder/file.hs``.

Om du vill lägga till alla filer som är untracked (troligtvis inte) skriver du ``git add .``

Om du vill lägga till filen ``Setup.hs`` skriver du:

```bash
$ git add Setup.hs
```

### commit
När du är redo att committa skriver du:

```bash
$ git commit -m "Ett meddelande"
```

Kom ihåg att skriva tydligt vad du gjort, i en mening (på engelska). Det går att skriva längre meddelanden (genom att använda ``git commit --amend`` efter att du redan commitat), men det är överkurs just nu.

### push
Efter att du committat, glöm inte att pusha ändringarna till repot på Github.com:

```bash
$ git push
```

Observera: Du behöver inte pusha varje gång du har commitat, utan du kan pusha flera commits.

### Branches
En bra sak med Git är branches. När man skapar en ny branch skapar man en kopia av repot (som den ser ut just då). Alla ändringar som görs i den kommer då att bara ske där.

Fördelen med branches är att man kan jobba med exempelvis en ny funktion, utan att riskera att programmet går sönder. Den fungerade koden finns ju master-branchen.

För att skapa en ny branch skriver du:

```bash
$ git branch branchName
```

För att byta till branch:

```bash
$ git checkout branchName
```
