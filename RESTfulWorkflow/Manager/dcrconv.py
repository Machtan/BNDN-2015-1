
import sys
import xml.etree.ElementTree as ET
import pprint

namemap = {
    "conditions":   "condition",
    "responses":    "response",
    "excludes":     "exclusion",
    "includes":     "inclusion",
}

RELATIONNAME    = "rel"
ROLENAME        = "rol"
EVENTNAME       = "eve"
USERNAME        = "use"

# role: Role <name>
# create: Event <name> <Role>
# relation: Relen <eventname> <type> <eventname>

def dcrconv(file):
    """docstring for dcrconv"""
    tree = ET.parse(file)
    root = tree.getroot()
    lines = [] # Lines for the new file

    events = {}

    specs = root[0]
    resources = specs[0]
    constraints = specs[1]
    
    users = {}
    all_roles = set()

    for role in resources.find("custom").find("roles"):
        name = role.text.replace(" ", "_")
        lines.append("{} {}".format(ROLENAME, name))
        all_roles.add(name)
        users[name] = [name]
    
    for name, roles in users.items():
        lines.append("{} {} {}".format(USERNAME, name, " ".join(roles)))
    lines.append("{} Test {}".format(USERNAME, " ".join(all_roles)))
    
    for event in resources.find("events"):
        eid = event.attrib["id"]
        roles = []
        for role in event.find("custom").find("roles"):
            if role.text:
                roles.append(role.text.replace(" ", "_"))
        #roles = [role.text.replace(" ", "_") for role in event.find("custom").find("roles")]
        #print("Adding {} to {}".format(roles, eid))
        roletext = " ".join(roles)
        events[eid] = [roletext]

    for label in resources.find("labelMappings"):
        eid = label.attrib["eventId"]
        labtext = label.attrib["labelId"].replace("\n", " ").replace(" ", "_")
        events[eid].append(labtext)

    # Find event states
    # excluded, pending, executed => 000
    marking = root.find("runtime").find("marking")
    included = {n.attrib["id"] for n in marking.find("included")}
    executed = {n.attrib["id"] for n in marking.find("executed")}
    pending = {n.attrib["id"] for n in marking.find("pendingResponses")}
    for eid in events:
        inc = "1" if eid in included else "0"
        pen = "1" if eid in pending else "0"
        exe = "1" if eid in executed else "0"
        events[eid].append(inc + pen + exe + "0")

    #pprint.pprint(events)
    for event, attrs in events.items():
        role = " " + attrs[0] if attrs[0] else ""
        name = attrs[1]
        state = attrs[2]
        lines.append("{} {} {}{}".format(EVENTNAME, state, name, role))

    for supertag, relname in namemap.items():
        for tag in constraints.find(supertag):
            source = events[tag.attrib["sourceId"]][1]
            target = events[tag.attrib["targetId"]][1]
            lines.append("{} {} {} {}".format(RELATIONNAME, source, relname, target))

    for line in lines:
        print(line)

def main(args=sys.argv[1:]):
    """Entry point"""
    if len(args) < 1:
        print("Usage: python3 dcrconv.py <xmlfile>")
    else:
        dcrconv(args[0])

if __name__ == '__main__':
    main()