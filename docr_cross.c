#include <stdio.h>

#include <fann.h>

void validate_network(struct fann * ann, struct fann_train_data *data);

struct fann_train_data *validate_data;
unsigned int num_layers;
char layers[200];

int FANN_API test_callback(struct fann *ann, struct fann_train_data *train,
                           unsigned int max_epochs, unsigned int epochs_between_reports,
                           float desired_error, unsigned int epochs)
{
  printf("Epochs     %8d. MSE: %.5f. Desired-MSE: %.5f\n", epochs, fann_get_MSE(ann), desired_error);
  validate_network(ann, validate_data);
  char fn[200];
  sprintf(fn, "digits_%dep_%dts_%dl_%s_ep%d.net", max_epochs, train->num_data, num_layers, layers, epochs);
  fann_save(ann, fn);
  return 0;
}

int main(int argc, char *argv[])
{

  if (argc < 2) {
    printf("Not enough params\n");
    exit(0);
  }

  const unsigned int num_input = 784; // 25*25
  const unsigned int num_output = 10;
  const float desired_error = (const float) 0.02;
  const unsigned int max_epochs = atoi(argv[1]);
  const unsigned int epochs_between_reports = 50;
  struct fann *ann;
  struct fann_train_data *data;
  int i;
  int x = 0;

  num_layers = (argc - 2) + 2;

  unsigned int * neurons = malloc(sizeof(unsigned int[num_layers]));
  neurons[0] = num_input;
  neurons[num_layers-1] = num_output;

  for (i = 2; i < argc; i++) {
    x = atoi(argv[i]);
    neurons[i-1] = x;
  }

  x = 0;
  for (i = 1; i < num_layers - 1; i++) {
    x += sprintf(&layers[x], "%dhu_", neurons[i]);
  }

  printf("Loading data...\n");

  data = fann_read_train_from_file("train_fann.txt");
  validate_data = fann_read_train_from_file("test_validate.data");

  char fn[100];
  sprintf(fn, "digits_%dep_%dts_%dl_%s.net", max_epochs, data->num_data, num_layers, layers);

  printf("Will write to: %s\n", fn);

  printf("Creating network.\n");
  ann = fann_create_standard_array(num_layers, neurons);

  /* fann_set_activation_steepness_hidden(ann, 1); */
  /* fann_set_activation_steepness_output(ann, 1); */

  /* fann_set_activation_function_hidden(ann, FANN_SIGMOID_SYMMETRIC);  */
  fann_set_activation_function_output(ann, FANN_SIGMOID);

  /* fann_set_train_stop_function(ann, FANN_STOPFUNC_BIT); */
  /* fann_set_bit_fail_limit(ann, 0.01f); */

  fann_set_callback(ann, test_callback);

  fann_init_weights(ann, data);

  printf("Training network.\n");
  fann_train_on_data(ann, data, max_epochs, epochs_between_reports, desired_error);

  fann_save(ann, fn);

  printf("Cleaning up.\n");
  fann_destroy_train(data);
  fann_destroy(ann);

  return 0;
}










void validate_network(struct fann * ann, struct fann_train_data *data) {
  fann_type *calc_out;
  int i, j;
  int good = 0;
  int bad = 0;

  for(i = 0; i < data->num_data; i++){
    calc_out = fann_test(ann, data->input[i], data->output[i]);
    int sb = -1;
    for (j = 0; j < 10; j++) {
      if (data->output[i][j] == 1) {
        sb = j;
      }
    }

    int res = -1;
    fann_type rv = -1;
    for (j = 0; j < 10; j++) {
      if (calc_out[j] > rv) {
        res = j;
        rv = calc_out[j];
      }
    }
    if (res == sb) {
      good++;
    } else {
      bad++;
    }
  }
  printf("Results on validation data: %d / %d, %f\n", good, (good+bad), (float)good / (good+bad));
  return;
}
