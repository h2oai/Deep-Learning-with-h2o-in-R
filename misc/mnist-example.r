 
#localH2O = h2o.init(); demo(h2o.glm)
# http://h2o.ai/
# http://learn.h2o.ai/content/hands-on_training/deep_learning.html
 
# Start H2O and load the MNIST data
#h2o.shutdown(h2oServer)
h2oServer <- h2o.init(nthreads=-1)

h2o.clusterStatus(h2oServer)

TRAIN = "train.csv"
TEST = "test.csv"
train_hex <- h2o.importFile(h2oServer, path = paste0(input.data.dir,"mnist/",TRAIN), header = F, sep = ',' )
test_hex <- h2o.importFile(h2oServer, path = paste0(input.data.dir,"mnist/",TEST), header = F, sep = ',' )

dlmodel <- h2o.deeplearning(x=1:784, y=785, data=train_hex, validation=test_hex,hidden=c(50,50), epochs=0.1, activation="Tanh")

dlmodel
dlmodel@model$confusion
dlmodel@model$valid_class_error

# confirm that the reported confusion matrix
pred_labels <- h2o.predict(dlmodel, test_hex)[,1]
actual_labels <- test_hex[,785]
cm <- h2o.confusionMatrix(pred_labels, actual_labels)
cm
dlmodel@model$confusion
dlmodel@model$confusion == cm

dlmodel@model$params

# Hyper-parameter Tuning with Grid Search

grid_search <- h2o.deeplearning(x=c(1:784), 
                                y=785, 
                                training_frame=train_hex, 
                                validation_frame=test_hex,
                                hidden=list(c(10,10),c(20,20)), 
                                epochs=0.1,
                                activation=c("Tanh", "Rectifier"), 
                                l1=c(0,1e-5))

grid_search

best_model <- grid_search@model[[1]]
best_model
best_params <- best_model@model$params
best_params$activation
best_params$hidden
best_params$l1

# Hyper-parameter Tuning with Random Search

models <- c()
for (i in 1:10) {
  rand_activation <- c("TanhWithDropout", "RectifierWithDropout")[sample(1:2,1)]
  rand_numlayers <- sample(2:5,1)
  rand_hidden <- c(sample(10:50,rand_numlayers,T))
  rand_l1 <- runif(1, 0, 1e-3)
  rand_l2 <- runif(1, 0, 1e-3)
  rand_dropout <- c(runif(rand_numlayers, 0, 0.6))
  rand_input_dropout <- runif(1, 0, 0.5)
  dlmodel <- h2o.deeplearning(x=1:784, y=785, 
                              data=train_hex, 
                              validation=test_hex, 
                              epochs=0.1,
                              activation=rand_activation,
                              hidden=rand_hidden, 
                              l1=rand_l1, 
                              l2=rand_l2,
                              input_dropout_ratio=rand_input_dropout, 
                              hidden_dropout_ratios=rand_dropout)
  models <- c(models, dlmodel)
}

#  find the model with the lowest validation error
best_err <- best_model@model$valid_class_error #best model from grid search above
for (i in 1:length(models)) {
  err <- models[[i]]@model$valid_class_error
  if (err < best_err) {
    best_err <- err
    best_model <- models[[i]]
  }
}

best_model
best_params <- best_model@model$params
best_params$activation
best_params$hidden
best_params$l1
best_params$l2
best_params$input_dropout_ratio
best_params$hidden_dropout_ratios

# continue training the best model, for 2 more epochs.
dlmodel_continued <- h2o.deeplearning(x=c(1:784), y=785, data=train_hex, validation=test_hex,
                                      checkpoint = best_model, l1=best_params$l1, l2=best_params$l2, epochs=0.5)

dlmodel_continued@model$valid_class_error

h2o.saveModel(dlmodel_continued, dir=paste0(output.data.dir, "saved-models/"), name="mybest_mnist_model", force=T)



