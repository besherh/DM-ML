import flask
import tensorflow as tf
import numpy as np
import json
# Importing socket library 
import socket 
  
# Function to display hostname and 
# IP address 
def get_Host_name_IP(): 
    try: 
        host_name = socket.gethostname() 
        host_ip = socket.gethostbyname(host_name) 
        return host_ip
    except: 
        print("Unable to get Hostname and IP")
# instantiate flask
app = flask.Flask(__name__)

def load_model(file_name):
    interpreter_quant = tf.lite.Interpreter(model_path=str(file_name))
    interpreter_quant.allocate_tensors()
    return interpreter_quant


def intialize_int_test_set():
    print("dataset loaded")
    _, cifar100_test = tf.keras.datasets.cifar10.load_data()
    images = tf.cast(cifar100_test[0], tf.uint8)
    labels = cifar100_test[1]
    cifar100_ds_uint8 = tf.data.Dataset.from_tensor_slices((images, labels)).batch(1)
    return cifar100_ds_uint8


file_name = 'cifar100.tflite'
interpreter = load_model(file_name)

def model_eval(interpreter_quant, test_set): 
  #for edge tpu
  #interpreter = Interpreter(model_path, experimental_delegates=[load_delegate('libedgetpu.so.1.0')])
    input_index = interpreter_quant.get_input_details()[0]["index"]
    output_index = interpreter_quant.get_output_details()[0]["index"]
    total_seen = 0
    model_prediction = []
    for img, label in test_set:
        print("image",total_seen)
        total_seen += 1
        interpreter_quant.set_tensor(input_index, img)
        interpreter_quant.invoke()
        predictions = interpreter_quant.get_tensor(output_index)
        model_prediction.append(predictions)
    return model_prediction

def default(obj):
    if type(obj).__module__ == np.__name__:
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        else:
            return obj.item()
    raise TypeError('Unknown type:', type(obj))

# define a predict function as an endpoint
@app.route("/predict", methods=["GET","POST"])
def predict():
    data = {"success": False}
    params = flask.request.json
    if params == None:
        params = flask.request.args
    # if parameters are found, return a prediction
    if (params != None):
        print("Data is not null")
        print(params['n'])
        global test_set
        test_set = test_set.take(int(params['n']))
        predictions = model_eval(interpreter, test_set)
        data = {"success": True, "Predictions" : predictions}
        dumped = json.dumps(data, default=default)

    # return a response in json format
    return flask.jsonify(dumped)


# start the flask app, allow remote connections
test_set = intialize_int_test_set()
app.run(host=get_Host_name_IP())
