#include <stdio.h>

#include <fann.h>

int FANN_API test_callback(struct fann *ann, struct fann_train_data *train,
                           unsigned int max_epochs, unsigned int epochs_between_reports,
                           float desired_error, unsigned int epochs)
{
  printf("Epochs     %8d. MSE: %.5f. Desired-MSE: %.5f\n", epochs, fann_get_MSE(ann), desired_error);
  return 0;
}

void test(char * fn, struct fann_train_data *data) {
  fann_type *calc_out;
  struct fann *ann;
  int i, j;

  ann = fann_create_from_file(fn);

  int good = 0;
  int bad = 0;

  int matrix[10][10];
  /* memset(matrix[0], 0, sizeof(int)); */
  for (i = 0; i < 10; i++) {
    for (j = 0; j < 10; j++) {
      matrix[i][j] = 0;
    }
  }

  for(i = 0; i < data->num_data; i++){
    fann_reset_MSE(ann);
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
    matrix[sb][res]++;
    if (res == sb) {
      good++;
    } else {
      bad++;
    }
    /* printf("Digits test %d -> %d, should be %d, conf %f\n", */
    /*        i, res, sb, rv); */
  }
  printf("Results: %d / %d, %f\n", good, (good+bad), (float)good / (good+bad));

  printf("Confusion matrix:\n");
  printf("      ");
  for (i = 0; i < 10; i++) {
    printf("%5d ", i);
  }
  printf("\n");
  for (i = 0; i < 10; i++) {
    printf("%5d ", i);
    for (j = 0; j < 10; j++) {
      printf("%5d ", matrix[i][j]);
    }
    printf("\n");
  }


  /* printf("Cleaning up.\n"); */
  fann_destroy(ann);

  return;
}

int main(int argc, char *argv[])
{
  struct fann_train_data *data;
  data = fann_read_train_from_file("test.data");
  int i;
  for (i = 1; i < argc; i++) {
    printf("NN: %s ", argv[i]);
    test(argv[i], data);
  }
  fann_destroy_train(data);
  return 0;
}
