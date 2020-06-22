(: Count number of occurrences of features in rooms across enterprises and order them in the descendingly. :)
let $enterprises := //enterprise
let $featureNames := fn:distinct-values($enterprises//roomType//feature/name/text())
for $featureName in $featureNames
let $roomTypesWithFeature := $enterprises//roomType[.//feature/name/text() = $featureName]
let $sum := fn:sum(
    for $roomType in $roomTypesWithFeature
    let $roomsOfRoomType := //room[@rtId-reference = $roomType/@rtId]
    return fn:count($roomsOfRoomType)
)
order by $sum descending
return element feature { 
    element name { $featureName },
    element appearences { $sum }
}
