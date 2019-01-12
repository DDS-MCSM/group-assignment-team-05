import os
import subprocess
import csv
import pprint
from multiprocessing import pool
import logging
import traceback

logging.basicConfig(level=logging.DEBUG)

def list_dirs():
    files = os.listdir(".")
    return [file for file in files if os.path.isdir(os.path.join(".", file))]

def build_queue(dir_name):
    files = [file for file in os.listdir(os.path.join(".", dir_name)) if file.endswith(".apk")]

    apk_files = []
    for filename in files:
        apk_files.append({"dirname": dir_name, "file": filename})
    return apk_files

def analyze_apk(arg):
    try:
        dir_name, filename = arg["dirname"], arg["file"]
        apk_path = os.path.join(".", dir_name, filename)
        data = {
            "name": filename,
            "type": dir_name
        }

        data["apk_size"] = analyzer("apk", "file-size", apk_path)
        data["download_size"] = analyzer("apk", "download-size", apk_path)
        data["n_features"] = analyzer("apk", "features", apk_path).count("\n") + 1
        data["n_files"] = analyzer("files", "list", apk_path).count("\n") + 1

        data["application_id"] = analyzer("manifest", "application-id", apk_path)
        data["version_name"] = analyzer("manifest", "version-name", apk_path)
        data["version_code"] = analyzer("manifest", "version-code", apk_path)
        data["min_sdk"] = analyzer("manifest", "min-sdk", apk_path)
        data["target_sdk"] = analyzer("manifest", "target-sdk", apk_path)
        data["n_permissions"] = analyzer("manifest", "permissions", apk_path).count("\n") + 1
        data["debuggable"] = analyzer("manifest", "debuggable", apk_path)

        data["n_dex"] = analyzer("dex", "list", apk_path).count("\n") + 1
        references_output = analyzer("dex", "references", apk_path)
        data["n_references"] = sum(int(line.split()[1]) for line in references_output.split("\n"))

        data["n_resources_packages"] = analyzer("resources", "packages", apk_path).count("\n") + 1
        data.update(handle_class_tree(apk_path))

        #logging.info(f"Analyzed apk {dir_name}/{filename}; data: {data}")
        return data
    except Exception as e:
        logging.error(f"Exception found when processing {dir_name}/{filename}: {e}; traceback: {traceback.print_tb(e.__traceback__)}")
        pass

def handle_class_tree(apk_path):
    output = analyzer("dex", "packages", "--show-removed", apk_path)
    max_depth = 0

    data = {
        "n_defined_packages": 0,
        "n_referenced_packages": 0,
        "n_kept_packages": 0,
        "n_removed_packages": 0,

        "n_defined_classes": 0,
        "n_referenced_classes": 0,
        "n_kept_classes": 0,
        "n_removed_classes": 0,

        "n_defined_methods": 0,
        "n_referenced_methods": 0,
        "n_kept_methods": 0,
        "n_removed_methods": 0,

        "n_defined_fields": 0,
        "n_referenced_fields": 0,
        "n_kept_fields": 0,
        "n_removed_fields": 0,

        "total_packages_size": 0,
        "total_classes_size": 0,
        "total_methods_size": 0,
        "total_fields_size": 0,
    }

    for out in output.split("\n"):
        mappings = {
            "P": "packages",
            "C": "classes",
            "M": "methods",
            "F": "fields",
            "d": "defined",
            "r": "referenced",
            "k": "kept",
            "x": "removed"
        }
        type_, state, defined_methods, referenced_methods, byte_size, *name = out.split()

        depth = name[0].count(".") + 1
        max_depth = max(depth, max_depth)

        property_name = f"n_{mappings[state]}_{mappings[type_]}"
        data[property_name] += 1
        data[f"total_{mappings[type_]}_size"] += int(byte_size)

    data["max_depth"] = max_depth
    return data


def analyzer(*args):
    output = subprocess.check_output(["apkanalyzer"] + list(args))
    return output.decode().strip()


def write_csv(name, results):
    headers = results[0].keys()
    with open(name, "w", newline="") as csv_file:
        writer = csv.writer(csv_file, delimiter=",", quotechar="\"", quoting=csv.QUOTE_MINIMAL)
        writer.writerow(headers)

        for result in results:
            writer.writerow([result[key] for key in headers])


if __name__ == "__main__":
    dirs = list_dirs()
    results = []

    apk_files = []
    for dir_ in dirs:
        apk_files += build_queue(dir_)

    pool = pool.Pool(8)
    pool.map(analyze_apk, apk_files)
    pool.close()
    pool.join()

    write_csv("data.csv", results)
