from locust import HttpLocust, task, TaskSet
from random import randrange

def random_element(xs):
    return xs[randrange(len(xs))]

def select_snapshot():
    _snapshots = [
        "lts",
        "nightly",
        "lts-9.10",
        "lts-9.7",
        "lts-9.6",
        "lts-9.5",
        "lts-8.8",
        "nightly-2017-07-05",
        "nightly-2017-05-30",
        "nightly-2017-03-25",
        "lts-7.20",
    ]
    return random_element(_snapshots)

def select_package():
    _packages = [
        "accelerate",
        "adjunctions",
        "aeson",
        "binary",
        "both",
        "extensible-effects",
        "hamlet",
        "hdocs",
        "microlens",
        "range",
        "sort",
        "text",
        "universe"
    ]
    return random_element(_packages)

def select_hoogle_query():
    _hoogle_queries = [
        "Ord",
        "Eq",
        "Num",
        "pack",
        "Text -> String",
        "fmap",
        "a -> a",
        "traverse",
        "bracket",
        "^.",
        ">>>",
        "<$>",
        "bimap",
        "inject"
    ]
    return random_element(_hoogle_queries)

class HoogleQueries(TaskSet):
    @task
    def hoogle_queries(self):
        _snapshot = select_snapshot()
        _query = select_hoogle_query()
        self.client.get("/" \
                        + _snapshot \
                        + "/hoogle?q=" + _query \
                        , name="/:snapshot/hoogle?q=[:query]")

    @task
    def stop(self):
        self.interrupt()

class Haddock(TaskSet):
    @task
    def haddock(self):
        _snapshot = select_snapshot()
        _package  = select_package()
        self.client.get("/haddock/" \
                        + _snapshot + "/" \
                        + _package + "/" \
                        + "doc-index-All.html" \
                        , name="/haddock/:snapshot/:package/doc-index-All.html")

    @task
    def stop(self):
        self.interrupt()

class Documentation(TaskSet):
    @task
    def docs(self):
        _snapshot = select_snapshot()
        self.client.get("/" \
                        + _snapshot \
                        + "/docs" \
                        , name="/:snapshot/docs")

    @task
    def stop(self):
        self.interrupt()

class PackageBrowser(TaskSet):
    @task
    def browse_package(self):
        _snapshot = select_snapshot()
        _package  = select_package()
        self.client.get("/" \
                        + _snapshot \
                        + "/package/" + _package \
                        , name="/:snapshot/package/:package")

    @task
    def stop(self):
        self.interrupt()

class Snapshots(TaskSet):
    @task
    def updateSnapshots(self):
        self.client.get("/download/snapshots.json")

    @task
    def stop(self):
        self.interrupt()

class TopLevelPages(TaskSet):
    @task(20)
    def install(self):
        self.client.get("/install")

    @task(10)
    def lts(self):
        self.client.get("/lts")

    @task(5)
    def nightly(self):
        self.client.get("/nightly")

    @task(2)
    def snapshots(self):
        self.client.get("/snapshots")

    @task(2)
    def stop(self):
        self.interrupt()

class UserBehaviour(TaskSet):
    tasks = {
        Haddock        : 10,
        HoogleQueries  : 5,
        PackageBrowser : 2,
        Documentation  : 2,
        Snapshots      : 1,
        TopLevelPages  : 1,
    }

class WebsiteUser(HttpLocust):
    task_set = UserBehaviour
    min_wait = 1000
    max_wait = 9000
