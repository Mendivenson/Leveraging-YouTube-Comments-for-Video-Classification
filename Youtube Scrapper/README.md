# Some Notes on the YouTube API

## About the Selected Categories

Each video category has a special attribute called `snippet.assignable`, which indicates whether that specific category can be assigned to a video by its author. Only categories with the `snippet.assignable` attribute set to `TRUE` can be queried or made searchable. Therefore, it is essential to select only these specific categories. The assignable categories are:

<div style="text-align: center;">

| ID |       Category        |
|:--:|:---------------------:|
| 1  |   Film & Animation    |
| 2  |   Autos & Vehicles    |
| 10 |         Music         |
| 15 |    Pets & Animals     |
| 17 |        Sports         |
| 19 |    Travel & Events    |
| 20 |        Gaming         |
| 22 |    People & Blogs     |
| 23 |        Comedy         |
| 24 |     Entertainment     |
| 25 |    News & Politics    |
| 26 |     Howto & Style     |
| 27 |       Education       |
| 28 | Science & Technology  |
| 29 | Nonprofits & Activism |

</div>

## About the Max Results in Each Category

This project primarily focuses on analyzing videos as content, so YouTube Shorts are excluded. Additionally, each video must have at least one comment to be considered. These constraints must be accounted for when building the database.

However, a significant limitation of the YouTube API is that it allows only a certain total number of results per category, even if the query is repeated multiple times. For example:
- The **Music** category has a limit of 30 results.
- Most other categories have a limit of 200 results.

As a result, there is a reasonable chance that some categories may end up with no videos after all filters are applied.

## About Comment Fetching

There are two primary goals for fetching comments:

1. Fetch the 50 most relevant comments for each video (or fewer if the video has less than 50 comments).
2. Fetch the replies to the top comment (sorted by relevance) for each video.
