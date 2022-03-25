wget -nc  https://bulkdata.uspto.gov/data/patent/ai/landscape/economics/2020_ai_model_predictions.tsv.zip 
unzip -n 2020_ai_model_predictions.tsv.zip

wget -nc https://s3.amazonaws.com/data.patentsview.org/download/application.tsv.zip
unzip -n application.tsv.zip

wget -nc https://s3.amazonaws.com/data.patentsview.org/download/patent.tsv.zip
unzip -n patent.tsv.zip

wget -nc https://s3.amazonaws.com/data.patentsview.org/download/rawinventor.tsv.zip
unzip -n rawinventor.tsv.zip

wget -nc https://s3.amazonaws.com/data.patentsview.org/download/rawassignee.tsv.zip
unzip -n rawassignee.tsv.zip

wget -nc https://s3.amazonaws.com/data.patentsview.org/download/rawlocation.tsv.zip
unzip -n rawlocation.tsv.zip

wget -nc https://s3.amazonaws.com/data.patentsview.org/download/location.tsv.zip
unzip -n location.tsv.zip

wget -nc https://s3.amazonaws.com/data.patentsview.org/download/uspatentcitation.tsv.zip
unzip -n uspatentcitation.tsv.zip
