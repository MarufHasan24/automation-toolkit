import rhinoscriptsyntax as rs
import random

# Select surfaces
surfaces = rs.GetObjects("Select surfaces", rs.filter.surface)

if surfaces:

    # Ask for offset values
    value_string = rs.GetString(
        "Enter offset values (example: 1,2,3,4)",
        "1,2,3,4"
    )

    if value_string:

        try:
            values = [float(x.strip()) for x in value_string.split(",")]

            # Ask for scale
            scale = rs.GetReal(
                "Enter scale factor",
                1.0,
                0.001
            )

            if scale:

                for srf in surfaces:

                    # Pick random value
                    original_distance = random.choice(values)

                    # Divide distance by scale
                    distance = original_distance / scale

                    # Select only this surface
                    rs.UnselectAllObjects()
                    rs.SelectObject(srf)

                    # OffsetSrf
                    command = (
                        "OffsetSrf "
                        "_Distance " + str(distance) + " "
                        "_Solid=Yes "
                        "_Enter"
                    )

                    rs.Command(command, True)

                    print(
                        "Original: {0} | Scale: {1} | Offset: {2}".format(
                            original_distance,
                            scale,
                            distance
                        )
                    )

                rs.UnselectAllObjects()
                print("Finished.")

        except:
            print("Invalid input.")