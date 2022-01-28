# credit @atpotts
import yaml
import json
import sys
input = yaml.safe_load(sys.stdin)
input["colors"] = {}
for (k,v) in list(input.copy().items()):
    if k[0:4] == "base":
        col = k[4:]
        input["colors"][k[4:]] = {
            "hex" : v,
            "red" : v[0:2],
            "green" : v[2:4],
            "blue" : v[4:6],
            "red-rgb" : int(v[0:2],16),
            "green-rgb" : int(v[2:4],16),
            "blue-rgb" : int(v[4:6],16),
           }
        input["base"+col+"-hex"] = input["colors"][col]["hex"]
        input["base"+col+"-hex-r"] = input["colors"][col]["red"]
        input["base"+col+"-hex-g"] = input["colors"][col]["green"]
        input["base"+col+"-hex-b"] = input["colors"][col]["blue"]
        input["base"+col+"-rgb-r"] = str(input["colors"][col]["red-rgb"])
        input["base"+col+"-rgb-g"] = str(input["colors"][col]["green-rgb"])
        input["base"+col+"-rgb-b"] = str(input["colors"][col]["blue-rgb"])
        input["base"+col+"-dec-r"] = str(input["colors"][col]["red-rgb"]/255.0)
        input["base"+col+"-dec-g"] = str(input["colors"][col]["green-rgb"]/255.0)
        input["base"+col+"-dec-b"] = str(input["colors"][col]["blue-rgb"]/255.0)
    elif k ==  "scheme":
      input["scheme-name"] = v
    else:
      input["scheme-"+k] = v
input["scheme-slug"]=sys.argv[1]
json.dump(input, sys.stdout)
