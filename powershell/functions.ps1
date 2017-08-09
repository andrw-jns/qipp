$Host.UI.RawUI.WindowTitle = "One day the world will acquiesce to my powershell abilities"

set-location "H:\QIPP\"
#cd "S:\Commissioning Intelligence And Strategy\Strategic Analytics\Projects 2015\QIPP\"

function prompt
{
    $m = 30 # maximum prompt length
    $str = $pwd.Path
    if ($str.length -ge $m)
    {
        # The prompt will begin with "...",
        # end with ">", and in between contain
        # as many of the path characters as will fit,
        # reading from the end of the path.
        $str = "..." + $str.substring($str.length - $m + 4)
    }
    "$str> "
}
	
function archive
(
	$Filter = ""
	, $FileType = ""
)
{
	$Filter = $Filter + "*"
	$Source = join-path (Get-Item -Path ".\" -Verbose).FullName $FileType
	$Destination = join-path $Source "Archive"
	get-childitem $Source -Filter $Filter| rename-item -newname {$_.BaseName+" - Archived "+(Get-Date -f yyyy-MM-dd-HHmm)+$_.Extension}
	get-childitem $Source -Filter $Filter| move-item -Destination $Destination	
}
#archive $file "SQL"

function runSQL
(
	$file = ""
	, $database = "AcuteDW"
	, $server = "CSU-SQL-03"
)
{
	$Base = (Get-Item -Path ".\" -Verbose).FullName
	$Source = join-path $Base "SQL"
	$Dest = join-path $Base "Data"

	$sql = $file + ".sql"
	$out = $file + ".csv"
	
	$Script = join-path $Source $sql
	$Output = join-path $Dest $out
	sqlcmd -C -S $server -d $database -i $Script  -o $Output -s "," -W 
}
#runSQL $file # only need to specify database when it's not AcuteDW.

