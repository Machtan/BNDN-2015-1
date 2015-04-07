
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
    
    for role in resources.find("custom").find("roles"):
        name = role.text.replace(" ", "_")
        lines.append("{} {}".format(ROLENAME, name))
    
    for event in resources.find("events"):
        eid = event.attrib["id"]
        roles = [role.text.replace(" ", "_") for role in event.find("custom").find("roles")]
        #print("Adding {} to {}".format(roles, eid))
        roletext = roles[0] # TODO more roles than 1?
        events[eid] = [roletext]
    
    for label in resources.find("labelMappings"):
        eid = label.attrib["eventId"]
        labtext = label.attrib["labelId"].replace("\n", " ").replace(" ", "_")
        events[eid].append(labtext)
    
    #pprint.pprint(events)
    for event, attrs in events.items():
        role = attrs[0]
        name = attrs[1]
        lines.append("{} {} {}".format(EVENTNAME, name, role))
    
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