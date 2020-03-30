$resp = (Invoke-WebRequest -UseBasicParsing "https://www.google.com/complete/search?client=hp&hl=en&xhr=t&q=$($args[0])").Content | ConvertFrom-Json
foreach ($item in $resp[1]) {
    $temp = $item[0] -replace "<[/]?b>", ""
    # &$39; --> '
    $temp -replace "&#39;", "'"
}
