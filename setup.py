from setuptools import find_packages, setup  # type: ignore

setup(
    name="hanson",
    version="0.0.0",
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        "click",
        "flask",
        "jinja2",
        "psycopg2",
        "waitress",
    ],
    scripts=[
        "app.py",
        "cli.py",
    ],
)
