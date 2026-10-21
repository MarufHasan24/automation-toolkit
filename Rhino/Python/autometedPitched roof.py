import rhinoscriptsyntax as rs
import Rhino
import Rhino.Geometry as rg
import scriptcontext as sc
import System
import math


# ============================================================
# SETTINGS
# ============================================================

DEFAULT_PITCH = 30.0
DEFAULT_OVERHANG = 0.30

# True  = building + roof become one block
# False = roof is created but no block is made
MAKE_BLOCKS = True

# True = original building is replaced by the block instance
# False = original building remains and block instance is added
REPLACE_BUILDING = True


# ============================================================
# BLOCK NAME
# ============================================================

block_counter = 1


def get_unique_block_name():

    global block_counter

    while True:

        name = "AUTO_ROOF_{0}".format(block_counter)

        block_counter += 1

        if sc.doc.InstanceDefinitions.Find(name, True) < 0:
            return name


# ============================================================
# FIND TOP FACE
# ============================================================

def find_top_face(brep):

    best_face = None
    best_z = -999999999.0

    for face in brep.Faces:

        # Get face area properties
        amp = rg.AreaMassProperties.Compute(face)

        if amp is None:
            continue

        center = amp.Centroid

        # Find UV on face
        ok, u, v = face.ClosestPoint(center)

        if not ok:
            continue

        normal = face.NormalAt(u, v)

        # We only want upward-facing faces
        if normal.Z < 0.90:
            continue

        if center.Z > best_z:

            best_z = center.Z
            best_face = face

    return best_face


# ============================================================
# GET OUTER BOUNDARY OF TOP FACE
# ============================================================

def get_outer_boundary(face):

    outer_loop = None

    for loop in face.Loops:

        if loop.LoopType == rg.BrepLoopType.Outer:

            outer_loop = loop
            break

    if outer_loop is None:
        return None

    curves = []

    for trim in outer_loop.Trims:

        edge = trim.Edge

        if edge is None:
            continue

        curve = edge.DuplicateCurve()

        if curve is not None:
            curves.append(curve)

    if not curves:
        return None

    joined = rg.Curve.JoinCurves(
        curves,
        sc.doc.ModelAbsoluteTolerance
    )

    if not joined:
        return None

    # Find closed curve
    for curve in joined:

        if curve.IsClosed:
            return curve

    return None


# ============================================================
# GET BUILDING ORIENTATION
# ============================================================

def get_building_direction(boundary):

    bbox = boundary.GetBoundingBox(True)

    dx = bbox.Max.X - bbox.Min.X
    dy = bbox.Max.Y - bbox.Min.Y

    # Long dimension becomes ridge direction
    if dx >= dy:

        direction = rg.Vector3d.XAxis

    else:

        direction = rg.Vector3d.YAxis

    direction.Unitize()

    return direction


# ============================================================
# GET CENTER OF FOOTPRINT
# ============================================================

def get_footprint_center(boundary):

    amp = rg.AreaMassProperties.Compute(boundary)

    if amp:

        center = amp.Centroid

        return rg.Point3d(
            center.X,
            center.Y,
            center.Z
        )

    bbox = boundary.GetBoundingBox(True)

    return bbox.Center


# ============================================================
# DISTANCE FROM POINT TO RIDGE LINE
# ============================================================

def perpendicular_distance(
        point,
        ridge_origin,
        ridge_direction):

    v = point - ridge_origin

    # perpendicular vector in XY
    perpendicular = rg.Vector3d(
        -ridge_direction.Y,
        ridge_direction.X,
        0
    )

    perpendicular.Unitize()

    distance = abs(
        rg.Vector3d.Multiply(
            v,
            perpendicular
        )
    )

    return distance


# ============================================================
# CREATE PLANAR ROOF BASE
# ============================================================

def create_roof_base(boundary, overhang):

    # No overhang
    if overhang <= 0.001:

        breps = rg.Brep.CreatePlanarBreps(
            boundary,
            sc.doc.ModelAbsoluteTolerance
        )

        if breps and len(breps) > 0:
            return breps[0]

        return None

    # Try to offset the building footprint
    offset_curves = boundary.Offset(
        rg.Plane.WorldXY,
        overhang,
        sc.doc.ModelAbsoluteTolerance,
        rg.CurveOffsetCornerStyle.Sharp
    )

    if not offset_curves:

        # If offset fails, use original footprint
        breps = rg.Brep.CreatePlanarBreps(
            boundary,
            sc.doc.ModelAbsoluteTolerance
        )

        if breps and len(breps) > 0:
            return breps[0]

        return None

    # Find closed offset curve
    closed_curves = []

    for curve in offset_curves:

        if curve.IsClosed:

            closed_curves.append(curve)

    if not closed_curves:

        breps = rg.Brep.CreatePlanarBreps(
            boundary,
            sc.doc.ModelAbsoluteTolerance
        )

        if breps and len(breps) > 0:
            return breps[0]

        return None

    # Usually the largest curve is the outside footprint
    largest_curve = None
    largest_area = -1

    for curve in closed_curves:

        amp = rg.AreaMassProperties.Compute(curve)

        if amp:

            area = abs(amp.Area)

            if area > largest_area:

                largest_area = area
                largest_curve = curve

    if largest_curve is None:
        largest_curve = closed_curves[0]

    breps = rg.Brep.CreatePlanarBreps(
        largest_curve,
        sc.doc.ModelAbsoluteTolerance
    )

    if breps and len(breps) > 0:

        return breps[0]

    return None


# ============================================================
# CREATE PITCHED ROOF
# ============================================================

def create_pitched_roof(
        roof_base,
        pitch,
        ridge_direction):

    # --------------------------------------------------------
    # Mesh the planar roof
    # --------------------------------------------------------

    meshing = rg.MeshingParameters()

    meshing.SimplePlanes = True
    meshing.RefineGrid = True
    meshing.MinimumEdgeLength = 0.01
    meshing.MaximumEdgeLength = 10.0

    meshes = rg.Mesh.CreateFromBrep(
        roof_base,
        meshing
    )

    if not meshes:
        return None

    roof_mesh = rg.Mesh()

    for mesh in meshes:

        roof_mesh.Append(mesh)

    if roof_mesh.Vertices.Count == 0:
        return None

    # --------------------------------------------------------
    # Find roof center
    # --------------------------------------------------------

    bbox = roof_mesh.GetBoundingBox(True)

    center = bbox.Center

    # Force center onto horizontal plane
    center.Z = bbox.Min.Z

    # --------------------------------------------------------
    # Perpendicular direction
    # --------------------------------------------------------

    perpendicular = rg.Vector3d(
        -ridge_direction.Y,
        ridge_direction.X,
        0
    )

    perpendicular.Unitize()

    # --------------------------------------------------------
    # Find maximum half-width
    # --------------------------------------------------------

    max_distance = 0.0

    for i in range(
        roof_mesh.Vertices.Count
    ):

        vertex = roof_mesh.Vertices[i]

        point = rg.Point3d(
            vertex.X,
            vertex.Y,
            vertex.Z
        )

        v = point - center

        distance = abs(
            rg.Vector3d.Multiply(
                v,
                perpendicular
            )
        )

        if distance > max_distance:

            max_distance = distance

    if max_distance <= 0.001:
        return None

    # --------------------------------------------------------
    # Roof pitch
    # --------------------------------------------------------

    pitch_radians = math.radians(pitch)

    slope = math.tan(pitch_radians)

    # --------------------------------------------------------
    # Move mesh vertices vertically
    # --------------------------------------------------------

    for i in range(
        roof_mesh.Vertices.Count
    ):

        vertex = roof_mesh.Vertices[i]

        point = rg.Point3d(
            vertex.X,
            vertex.Y,
            vertex.Z
        )

        v = point - center

        distance = abs(
            rg.Vector3d.Multiply(
                v,
                perpendicular
            )
        )

        # Distance from ridge
        #
        # ridge = highest
        # edge  = lowest
        #
        height = (
            max_distance - distance
        ) * slope

        new_z = vertex.Z + height

        roof_mesh.Vertices.SetVertex(
            i,
            vertex.X,
            vertex.Y,
            new_z
        )

    roof_mesh.Normals.ComputeNormals()
    roof_mesh.Compact()

    return roof_mesh


# ============================================================
# ADD ROOF TO DOCUMENT
# ============================================================

def add_roof_mesh(mesh):

    attributes = Rhino.DocObjects.ObjectAttributes()

    attributes.LayerIndex = get_roof_layer()

    roof_id = sc.doc.Objects.AddMesh(
        mesh,
        attributes
    )

    return roof_id


# ============================================================
# ROOF LAYER
# ============================================================

def get_roof_layer():

    layer_name = "AUTO_ROOFS"

    layer_index = sc.doc.Layers.Find(
        layer_name,
        True
    )

    if layer_index >= 0:

        return layer_index

    layer = Rhino.DocObjects.Layer()

    layer.Name = layer_name

    return sc.doc.Layers.Add(layer)


# ============================================================
# CREATE BLOCK
# ============================================================

def create_building_block(
        building_id,
        roof_id):

    if not MAKE_BLOCKS:
        return None

    block_name = get_unique_block_name()

    object_ids = [
        building_id,
        roof_id
    ]

    # Base point
    base_point = rg.Point3d.Origin

    # Add block definition
    block_index = sc.doc.InstanceDefinitions.Add(
        block_name,
        "Automatic building with pitched roof",
        base_point,
        object_ids
    )

    if block_index < 0:

        print(
            "Could not create block:",
            block_name
        )

        return None

    # Create instance
    instance_attributes = Rhino.DocObjects.ObjectAttributes()

    instance_id = sc.doc.Objects.AddInstanceObject(
        block_index,
        rg.Transform.Identity,
        instance_attributes
    )

    if REPLACE_BUILDING:

        # Delete source objects
        sc.doc.Objects.Delete(
            building_id,
            True
        )

        sc.doc.Objects.Delete(
            roof_id,
            True
        )

    return instance_id


# ============================================================
# PROCESS ONE BUILDING
# ============================================================

def process_building(
        building_id,
        pitch,
        overhang):

    try:

        # ----------------------------------------------------
        # Convert Rhino object to Brep
        # ----------------------------------------------------

        brep = rs.coercebrep(
            building_id
        )

        if brep is None:

            print(
                "Skipping object:",
                building_id
            )

            return False

        # ----------------------------------------------------
        # Find highest horizontal face
        # ----------------------------------------------------

        top_face = find_top_face(
            brep
        )

        if top_face is None:

            print(
                "No top face:",
                building_id
            )

            return False

        # ----------------------------------------------------
        # Get footprint
        # ----------------------------------------------------

        boundary = get_outer_boundary(
            top_face
        )

        if boundary is None:

            print(
                "Could not find footprint:",
                building_id
            )

            return False

        # ----------------------------------------------------
        # Get ridge direction
        # ----------------------------------------------------

        ridge_direction = (
            get_building_direction(
                boundary
            )
        )

        # ----------------------------------------------------
        # Get center
        # ----------------------------------------------------

        center = get_footprint_center(
            boundary
        )

        # Make sure ridge center is at actual top Z
        amp = rg.AreaMassProperties.Compute(
            top_face
        )

        if amp:

            center.Z = amp.Centroid.Z

        # ----------------------------------------------------
        # Create roof base
        # ----------------------------------------------------

        roof_base = create_roof_base(
            boundary,
            overhang
        )

        if roof_base is None:

            print(
                "Could not create roof base:",
                building_id
            )

            return False

        # ----------------------------------------------------
        # Move roof base to building top elevation
        # ----------------------------------------------------

        base_bbox = roof_base.GetBoundingBox(True)

        top_z = center.Z

        move_z = top_z - base_bbox.Min.Z

        if abs(move_z) > 0.000001:

            roof_base.Transform(
                rg.Transform.Translation(
                    0,
                    0,
                    move_z
                )
            )

        # ----------------------------------------------------
        # Create pitched roof mesh
        # ----------------------------------------------------

        roof_mesh = create_pitched_roof(
            roof_base,
            pitch,
            ridge_direction
        )

        if roof_mesh is None:

            print(
                "Could not create roof:",
                building_id
            )

            return False

        # ----------------------------------------------------
        # Add roof
        # ----------------------------------------------------

        roof_id = add_roof_mesh(
            roof_mesh
        )

        if roof_id == System.Guid.Empty:

            print(
                "Roof could not be added:",
                building_id
            )

            return False

        # ----------------------------------------------------
        # Create block
        # ----------------------------------------------------

        if MAKE_BLOCKS:

            block_id = create_building_block(
                building_id,
                roof_id
            )

            if block_id is None:

                # Keep roof if block failed
                print(
                    "Block failed:",
                    building_id
                )

                return False

        return True

    except Exception as error:

        print(
            "ERROR:",
            building_id,
            str(error)
        )

        return False


# ============================================================
# MAIN
# ============================================================

def main():

    print("")
    print("============================================")
    print("      AUTOMATIC PITCHED ROOF GENERATOR")
    print("                 RHINO 8")
    print("============================================")
    print("")

    # --------------------------------------------------------
    # Select buildings
    # --------------------------------------------------------

    objects = rs.GetObjects(
        "Select 3D building polysurfaces",
        rs.filter.polysurface,
        preselect=True
    )

    if not objects:

        print("Nothing selected.")
        return

    # --------------------------------------------------------
    # Roof pitch
    # --------------------------------------------------------

    pitch = rs.GetReal(
        "Roof pitch in degrees",
        DEFAULT_PITCH,
        5.0,
        60.0
    )

    if pitch is None:
        return

    # --------------------------------------------------------
    # Roof overhang
    # --------------------------------------------------------

    overhang = rs.GetReal(
        "Roof overhang",
        DEFAULT_OVERHANG,
        0.0
    )

    if overhang is None:
        return

    # --------------------------------------------------------
    # Confirm
    # --------------------------------------------------------

    print("")
    print("--------------------------------------------")
    print("Buildings:", len(objects))
    print("Roof pitch:", pitch, "degrees")
    print("Overhang:", overhang)
    print("Create blocks:", MAKE_BLOCKS)
    print("Replace buildings:", REPLACE_BUILDING)
    print("--------------------------------------------")
    print("")

    # --------------------------------------------------------
    # Process
    # --------------------------------------------------------

    rs.EnableRedraw(False)

    successful = 0
    failed = 0

    total = len(objects)

    for i, obj_id in enumerate(objects):

        result = process_building(
            obj_id,
            pitch,
            overhang
        )

        if result:

            successful += 1

        else:

            failed += 1

        # Progress every 25 buildings
        if (
            (i + 1) % 25 == 0
            or
            (i + 1) == total
        ):

            print(
                "Progress: {0}/{1} | Success: {2} | Failed: {3}".format(
                    i + 1,
                    total,
                    successful,
                    failed
                )
            )

    rs.EnableRedraw(True)

    sc.doc.Views.Redraw()

    print("")
    print("============================================")
    print("                 FINISHED")
    print("============================================")
    print("Total:", total)
    print("Successful:", successful)
    print("Failed:", failed)
    print("============================================")
    print("")


# ============================================================
# RUN
# ============================================================

if __name__ == "__main__":

    main()