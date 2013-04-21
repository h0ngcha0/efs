// query the json file

function(value, keyData, arg) {
    json = value;
    var se = arg.se;
    if (arg.sw < json.center_lng && json.center_lng > arg.se) {
        if (arg.nw < json.center_lat && json.center_lat > arg.ne) {
            return [json];
        }
    }
    return [];
}
