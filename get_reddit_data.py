#%%
# GET DATA
import praw
from praw.models import MoreComments
from psaw import PushshiftAPI

import datetime as dt
import pandas as pd

from IPython.display import clear_output

#%%
# FUNCTIONS TO EXTRACT POSTS INFORMATION
def get_submission_info(submission_obj):
    """
    Based on a submission object (reddit.submission),
    get important atributtes and return a dictionary
    with this information. Make sure that the dict has the same
    keys as in 'get_comments_info'.
    """
    submissions_dict = {
        'id' : submission_obj.id,
        'parent_id' : None,
        'title' : submission_obj.title, 
        'body': submission_obj.selftext,
        'time' : submission_obj.created_utc,
        'num_comments' : submission_obj.num_comments,
        'num_upvotes' : submission_obj.ups
        }

    return submissions_dict

def get_comments_info(comment_obj):
    """
    Based on a comment object (submission.comments.list()[0]),
    get important atributtes and return a dictionary
    with this information. Make sure that the dict has the same
    keys as in 'get_submission_info'.
    """
    comment_dict = {
        'id' : comment_obj.id,
        'parent_id' : comment_obj.parent_id,
        'title' : None,
        'body' : comment_obj.body,
        'time' : comment_obj.created_utc,
        'num_comments' : None,
        'num_upvotes' : comment_obj.ups
        }

    return comment_dict

#%%
# SUBMISSION ID FOR EVERY POST 
## to use PSAW
api = PushshiftAPI()

## Time interval
ts_after = int(dt.datetime(2021, 1, 1).timestamp()) # Start date
ts_before = int(dt.datetime(2022, 10, 1).timestamp()) # End date

## use PSAW only to get id of submissions in time interval
gen = api.search_submissions(
    after=ts_after,
    before=ts_before,
    filter=['id'],
    subreddit='Bitcoin',
    limit=None
)

submission_id_list = [submission_psaw.d_['id'] for submission_psaw in gen]

#%%
# REDDIT POSTS INFORMATION
## Connect to praw API
reddit = praw.Reddit(
    client_id='0sv3AGeethtp1aEAkj7LaA', 
    client_secret='a74EDTHgK5B71dg0E3Y553W3atnZnw',
    user_agent='crypto'
    )

sub_comm_list = []
for submission_id in submission_id_list:
    ## Progress bar
    clear_output()
    print(round(submission_id_list.index(submission_id) / len(submission_id_list) * 100, 2))
    try:
        submission = reddit.submission(id=submission_id)

        sub_comm_list.append(get_submission_info(submission)) 

        # Allow for all comments to be extracted
        submission.comments.replace_more(limit=None)
        for comment in submission.comments.list():
            sub_comm_list.append(get_comments_info(comment)) 

        final_df = pd.DataFrame(sub_comm_list)
        final_df.to_csv('reddit_posts.csv')
    except:
        print(f'{submission_id} not found')