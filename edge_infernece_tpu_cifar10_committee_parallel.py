import flask
import tensorflow as tf
import numpy as np
import json
import time
import logging

from tensorflow.lite.python import interpreter
from tensorflow.lite.python.interpreter import load_delegate
import glob

from threading import Thread

# Importing socket library
import socket
model_pool_prediction = {}
threads = []

# IP address 
def getNetworkIp():
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
    s.connect(('<broadcast>', 0))
    return s.getsockname()[0]
getNetworkIp()
# instantiate flask
app = flask.Flask(__name__)

ind_dir = './individual/'
together_dir = './together/'
def file_browser(directory_name):
  return [model for model in glob.glob(directory_name + "*.tflite")]

def intialize_int_test_set():
    print("dataset loaded")
    _, cifar100_test = tf.keras.datasets.cifar10.load_data()
    cifar100_test = cifar100_test
    images = tf.cast(cifar100_test[0], tf.uint8)
    labels = cifar100_test[1]
    cifar100_ds_uint8 = tf.data.Dataset.from_tensor_slices((images, labels)).batch(1)
    return cifar100_ds_uint8


def model_eval(model_name, test_set): 
  
    interpreter_quant = tf.lite.Interpreter(model_name, experimental_delegates=[load_delegate('libedgetpu.so.1.0')])
    interpreter_quant.allocate_tensors()
    input_index = interpreter_quant.get_input_details()[0]["index"]
    output_index = interpreter_quant.get_output_details()[0]["index"]
    total_seen = 0
    print(model_name)
    model_prediction = []
    for img, label in test_set:
        print("image",total_seen)
        total_seen += 1
        interpreter_quant.set_tensor(input_index, img)
        interpreter_quant.invoke()
        predictions = interpreter_quant.get_tensor(output_index)
        model_prediction.append(predictions)
    global model_pool_prediction
    model_pool_prediction[model_name] = model_prediction

def combine_predictions(test_samples, model_pool_dic):
  i = 0
  yhats = []
  while i <= test_samples-1:
    one_instance_prediction = []
    for key in model_pool_dic:
      one_instance_prediction.append(model_pool_dic[key][i])
    temp = np.array(one_instance_prediction)
    yhat = np.sum(temp, axis=0)
    yhats.append(yhat)  
    i += 1
  return np.array(yhats)

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
        global test_set
        test_set = test_set.take(int(params['n']))
        models_list = file_browser(together_dir)
        model_pool_prediction = {}
        start_time = time.time()
        for model_name in models_list:
            process = Thread(target=model_eval, args=[model_name, test_set])
            process.start()
            threads.append(process)
        for index, thread in enumerate(threads):
            logging.info("Main    : before joining thread %d.", index)
            thread.join()
            logging.info("Main    : thread %d done", index)
        yhats = combine_predictions(int(params['n']), model_pool_prediction)
        end_time = time.time()
        print('inference time for combininb predictions on edge: {0} : {1}'.format(getNetworkIp(),end_time-start_time))
        data = {"success": True, "Predictions" : yhats}
        dumped = json.dumps(data, default=default)

    # return a response in json format
    return flask.jsonify(dumped)


# start the flask app, allow remote connections
test_set = intialize_int_test_set()
app.run(host=getNetworkIp())
#app.run('127.0.0.1')
