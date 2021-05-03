(: A room is deemed luxurious if it has a sauna as its feature - get a list of rooms for each enterprise with a visibly disctinct rooms that are luxurious. :)
let $enterprises := fn:doc('../data.xml')//enterprise
for $enterprise in $enterprises
let $luxuryRoomTypes := $enterprise//roomType[.//feature/name="Sauna"]
let $rooms := $enterprise//room
return element enterprise {
    element name {
        text {$enterprise/name}
    },
    element rooms {
        for $room in $rooms
        return
            if (some $roomType in $luxuryRoomTypes satisfies $roomType/@rtId = $room/@rtId-reference)
            then element luxuryRoom { element roomNumber { $room/@roomNumber} }
            else element room { element roomNumber { $room/@roomNumber} }   
    }
}