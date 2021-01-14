class Group
{
    members;
    count;
    created;
    index;

    constructor(members)
    {
        this.members = members;
        this.count = this.members.length;
        this.created = this.members[0].created;
        this.index = this.members[0].index;
    }
}

function preprocessGalleryData(imgData)
{
    let indexedData = imgData.map(function(data, index) {
        data.index = index;
        return data;
    })

    let done = [];
    let groups = [];
    indexedData.forEach(data =>
    {
        let members = [];
        addSimilar(done, members, data, groups);
        safeAddNewGroup(groups, members)
    });

    groups.sort(comparator);

    let readyGroups = [];
    let coalescedGroupMembers = [];
    groups.forEach(group =>
    {
        if (group.count > 1)
        {
            safeAddNewGroup(readyGroups, coalescedGroupMembers);
            coalescedGroupMembers = [];
            readyGroups.push(group);
        }
        else
        {
            coalescedGroupMembers.push(group.members[0]);
        }
    });
    safeAddNewGroup(readyGroups, coalescedGroupMembers, imgData);

    readyGroups.sort(comparator);

    let groupMembers = readyGroups.map(group => group.members);
    return groupMembers;
}

function addSimilar(done, members, current, groups)
{
    if (!done.includes(current))
    {
        members.push(current);
        done.push(current);
        current.similar.forEach(similar => addSimilar(done, members, similar, groups));
    }
    else
    {
        if (!members.includes(current))
        {
            // "late comer" - references something already grouped
            let index = groups.findIndex(g => g.members.includes(current));
            let existingGroup = groups[index];
            console.log(existingGroup);
            existingGroup.members.forEach(member => members.push(member));
            groups.splice(index, 1);
        }
        else
        {
            // would be a cycle: is done and is in members
        }
    }
}

function comparator(objectA, objectB)
{
    let timeCompare = compareCreatedProperty(objectA, objectB)
    return timeCompare !== 0 ? timeCompare : compareIndexProperty(objectA, objectB);
}

function compareCreatedProperty(objectA, objectB)
{
    let dateA = objectA.created.getTime();
    let dateB = objectB.created.getTime();

    return dateA < dateB ? -1 : (dateA > dateB ? 1 : 0);
}

function compareIndexProperty(objectA, objectB)
{
    return objectA.index < objectB.index ? -1 : 1;
}

function safeAddNewGroup(groups, members)
{
    if (members.length > 0)
    {
        members.sort(comparator);
        let group = new Group(members);
        groups.push(group);
    }
}


// In nodejs, this is the way how export is performed.
// In browser, module has to be a global varibale object.
module.exports = { preprocessGalleryData };
