class Group
{
    members;
    count;
    created;
    index;

    constructor(members, index)
    {
        this.members = members;
        this.count = this.members.length;
        this.created = this.members[0].created;
        this.index = index;
    }
}

function preprocessGalleryData(imgData)
{
    let done = [];
    let groups = [];
    imgData.forEach(data =>
    {
        let members = [];
        addSimilar(done, members, data, groups);
        safeAddNewGroup(groups, members, imgData)
    });

    groups.sort(function(a, b) {
        let timeCompare = compareCreatedProperty(a, b);
        return timeCompare !== 0 ? timeCompare : (a.index < b.index ? -1 : 1);
    });

    let readyGroups = [];
    let coalescedGroupMembers = [];
    groups.forEach(group =>
    {
        if (group.count > 1)
        {
            safeAddNewGroup(readyGroups, coalescedGroupMembers, imgData);
            coalescedGroupMembers = [];
            readyGroups.push(group);
        }
        else
        {
            coalescedGroupMembers = coalescedGroupMembers.concat(group.members);
        }
    });
    safeAddNewGroup(readyGroups, coalescedGroupMembers, imgData);

    let groupMembers = readyGroups.map(group => group.members);
    return groupMembers;
}

function addSimilar(done, members, current, groups)
{
    if (!done.includes(current))
    {
        members.push(current);
        done.push(current);
        current.similar.forEach(similar =>
        {
            if (done.includes(similar) && !members.includes(similar))
            {
                let index = groups.findIndex(g => g.members.includes(similar));
                let group = groups[index];
                members = members.concat(group.members);
                groups.splice(index, 1);
            }
            else
            {
                addSimilar(done, members, similar, groups);
            }
        });
    }
}

function compareCreatedProperty(objectA, objectB)
{
    let dateA = new Date(objectA.created).getTime();
    let dateB = new Date(objectB.created).getTime();

    return dateA < dateB ? -1 : (dateA > dateB ? 1 : 0);
}

function safeAddNewGroup(groups, members, array)
{
    if (members.length > 0)
    {
        members.sort(function(a, b) {
            let timeCompare = compareCreatedProperty(a, b);
            return timeCompare !== 0 ? timeCompare : compareByPosition(a, b, array)
        });
        let group = new Group(members, array.indexOf(members[0]));
        groups.push(group);
    }
}

function compareByPosition(objectA, objectB, array)
{
    let positionA = array.indexOf(objectA);
    let positionB = array.indexOf(objectB);

    return positionA < positionB ? -1 : 1;
}


// In nodejs, this is the way how export is performed.
// In browser, module has to be a global varibale object.
module.exports = { preprocessGalleryData };
