import rhinoscriptsyntax as rs
import random
import math


def scatter_trees():

    # ---------------------------------------------------------
    # SELECT SURFACE
    # ---------------------------------------------------------
    surface = rs.GetObject(
        "Select surface to scatter trees on",
        rs.filter.surface
    )

    if not surface:
        return


    # ---------------------------------------------------------
    # SELECT TREE BLOCK
    # ---------------------------------------------------------
    tree = rs.GetObject(
        "Select tree block",
        rs.filter.instance
    )

    if not tree:
        return

    block_name = rs.BlockInstanceName(tree)

    if not block_name:
        print("Selected object is not a block.")
        return


    # ---------------------------------------------------------
    # NUMBER OF TREES
    # ---------------------------------------------------------
    number = rs.GetInteger(
        "Number of trees",
        100,
        1
    )

    if not number:
        return


    # ---------------------------------------------------------
    # RANDOM SCALE
    # ---------------------------------------------------------
    min_scale = rs.GetReal(
        "Minimum tree scale",
        0.7,
        0.01
    )

    if min_scale is None:
        return


    max_scale = rs.GetReal(
        "Maximum tree scale",
        1.3,
        min_scale
    )

    if max_scale is None:
        return


    # ---------------------------------------------------------
    # RANDOM ROTATION
    # ---------------------------------------------------------
    min_rotation = rs.GetReal(
        "Minimum rotation angle",
        0
    )

    if min_rotation is None:
        return


    max_rotation = rs.GetReal(
        "Maximum rotation angle",
        360,
        min_rotation
    )

    if max_rotation is None:
        return


    # ---------------------------------------------------------
    # MINIMUM DISTANCE
    # ---------------------------------------------------------
    min_distance = rs.GetReal(
        "Minimum distance between trees",
        2.0,
        0
    )

    if min_distance is None:
        return


    # ---------------------------------------------------------
    # SURFACE DOMAIN
    # ---------------------------------------------------------
    u_domain = rs.SurfaceDomain(surface, 0)
    v_domain = rs.SurfaceDomain(surface, 1)

    if not u_domain or not v_domain:
        print("Could not read surface domain.")
        return


    # ---------------------------------------------------------
    # STORE TREE POINTS
    # ---------------------------------------------------------
    points = []

    created = 0
    attempts = 0

    # Maximum attempts prevents infinite loop
    max_attempts = number * 100


    # ---------------------------------------------------------
    # HIDE ORIGINAL TREE
    # ---------------------------------------------------------
    rs.HideObject(tree)


    # ---------------------------------------------------------
    # GENERATE TREES
    # ---------------------------------------------------------
    while created < number and attempts < max_attempts:

        attempts += 1


        # Random UV position
        u = random.uniform(
            u_domain[0],
            u_domain[1]
        )

        v = random.uniform(
            v_domain[0],
            v_domain[1]
        )


        # Point on surface
        point = rs.EvaluateSurface(
            surface,
            u,
            v
        )

        if not point:
            continue


        # -----------------------------------------------------
        # CHECK MINIMUM DISTANCE
        # -----------------------------------------------------
        valid = True

        for old_point in points:

            distance = rs.Distance(
                point,
                old_point
            )

            if distance < min_distance:

                valid = False
                break


        if not valid:
            continue


        # -----------------------------------------------------
        # SURFACE NORMAL
        # -----------------------------------------------------
        normal = rs.SurfaceNormal(
            surface,
            (u, v)
        )

        if not normal:
            continue


        # -----------------------------------------------------
        # RANDOM SCALE
        # -----------------------------------------------------
        scale = random.uniform(
            min_scale,
            max_scale
        )


        # -----------------------------------------------------
        # INSERT TREE
        # -----------------------------------------------------
        new_tree = rs.InsertBlock(
            block_name,
            point,
            (scale, scale, scale),
            0
        )

        if not new_tree:
            continue


        # -----------------------------------------------------
        # RANDOM ROTATION
        # -----------------------------------------------------
        rotation = random.uniform(
            min_rotation,
            max_rotation
        )


        rs.RotateObject(
            new_tree,
            point,
            rotation,
            normal
        )


        # Save point
        points.append(point)

        created += 1


    # ---------------------------------------------------------
    # RESULT
    # ---------------------------------------------------------
    print("--------------------------------")
    print("TREE SCATTER COMPLETE")
    print("Requested : {}".format(number))
    print("Created   : {}".format(created))
    print("Attempts  : {}".format(attempts))
    print("--------------------------------")


scatter_trees()