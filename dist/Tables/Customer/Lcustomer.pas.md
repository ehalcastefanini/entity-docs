<!-- tabs:start -->

#### **Documentation**

# Documentation for `Lcustomer` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `Lcustomer` code unit is designed to manage and display a list of customers in a grid-based interface. It provides functionalities for searching, viewing, modifying, and managing customer-related data, including portal information, cross-selling brands, and various customer permissions. The form allows users to interact with customer data efficiently, offering search criteria, action buttons, and detailed customer information.

### Technologies Used:
- **Delphi VCL Components**: Used for creating the user interface and handling events.
- **Database Components**: Includes `DBClient` and `TDataSet` for database interaction.
- **Third-party Libraries**: Includes `TsPanel`, `TsButton`, `TcxGrid`, and other components for enhanced UI and functionality.
- **Custom Components**: Includes `kneCBListSOA`, `kneFRGridManager`, and `FRAMEfindCriteriaListCustomer` for specialized functionalities.

### Form Type:
This is a **grid display** form with additional form elements for customer details and actions.

#### Grid Columns:
1. **Customer Code** (type: string)
2. **Abbreviated Name** (type: string)
3. **Full Name** (type: string)
4. **Language Code** (type: string)
5. **Country Code** (type: string)
6. **Country Description** (type: string)
7. **Last Updated** (type: datetime)
8. **Updated By** (type: string)

#### Grid Actions:
1. **New**: Opens a form to create a new customer.
2. **Modify**: Opens a form to edit the selected customer.
3. **View**: Opens a form to view the selected customer in read-only mode.
4. **Search Area**: Toggles the search criteria panel.
5. **Advanced Search**: Opens advanced search options.
6. **Address and Documents**: Opens a form to manage customer addresses and documents.

---

## 2. Functionality Description:

### User Actions:
1. **Search Customers**: Users can search for customers using various criteria.
2. **View Customer Details**: Displays detailed information about a selected customer.
3. **Edit Customer Information**: Allows modification of customer data.
4. **Manage Portal Information**: Enables editing of portal-related settings for a customer.
5. **Cross-Selling Brands**: Manages cross-selling brand preferences for a customer.

### Main Components:
- **Grid Display**: Displays the list of customers.
- **Search Panel**: Allows users to filter customers based on criteria.
- **Action Buttons**: Provides options to create, modify, view, and manage customer data.
- **Portal Information Panel**: Displays and edits portal-related settings.

### Pseudo-code for Actions and Events:
- `OnClick` event of "New" button: `if button clicked then open new customer form`.
- `OnClick` event of "Modify" button: `if button clicked and customer selected then open edit form`.
- `OnClick` event of "View" button: `if button clicked and customer selected then open view form`.
- `OnClick` event of "Search" button: `if button clicked then execute search with criteria`.
- `OnChange` event of search fields: `if field value changed then update search criteria`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**: The form is initialized, and the customer grid is populated with data from the database.
2. **User Interaction**:
   - Users can search for customers using the search panel.
   - Clicking on a customer in the grid displays detailed information.
   - Action buttons allow users to create, modify, or view customer data.
3. **Event Handling**:
   - `FormShow`: Loads initial data and sets up the interface.
   - `ACTaddressAndDocExecute`: Opens the address and document management form.
   - `BTNokClick`: Saves changes made to customer data.
   - `BTNcancelClick`: Cancels any unsaved changes.

### Required Data:
- Search criteria (e.g., customer code, name, country).
- Customer details for editing or creating new entries.

---

## 4. Business Rules:

### Actions and Preconditions:
- **New**: Enabled at all times.
- **Modify**: Enabled only when a customer is selected.
- **View**: Enabled only when a customer is selected.
- **Search**: Requires at least one search criterion to be filled.

### Available Filters:
- Customer Code
- Name
- Country
- Status
- Business Channel

### Error Messages:
- "No customer selected" if an action requires a selected customer but none is selected.
- "Invalid input" if a field value does not meet validation criteria.

### Default Field Values:
- Status: Default to "Active".
- Language Code: Default to "EN".
- Country Code: Default to the user's country.

### Field Validation and Conditions:
- **Customer Code**: Required, alphanumeric, max 10 characters.
- **Name**: Required, string, max 50 characters.
- **Email**: Must be a valid email format.
- **Country Code**: Required, must match a predefined list of country codes.

---

## 5. Main Functions:

1. **FormShow**: Initializes the form and loads customer data.
2. **ACTaddressAndDocExecute**: Opens the address and document management form.
3. **BTNokClick**: Saves changes to customer data.
4. **BTNcancelClick**: Cancels changes and closes the form.

---

## 6. API Service Consumption:

- **Service Name**: CustomerService
- **Endpoint**: `/api/customers`
- **Data Sent**: `{ "customerCode": "string", "name": "string", "countryCode": "string" }`
- **Data Received**: `{ "status": "success", "data": "Customer object" }`
- **Purpose**: Fetch, create, or update customer data.
- **Error Handling**: Displays error messages if the API call fails.

---

## 7. Conditional Fields (Form Logic):

- **Portal Information Panel**: Visible only when the "Portal Flag" checkbox is checked.
- **Cross-Selling Brands Tab**: Enabled only when "Allow Cross-Selling" is checked.

---

## 8. Dependencies:

### External Libraries:
- `TsPanel`, `TsButton`, `TcxGrid`: Used for UI components.
- `kneCBListSOA`, `kneFRGridManager`: Custom components for grid management and search functionality.

### Custom Components:
- `FRAMEfindCriteriaListCustomer`: Handles advanced search criteria.

---

## 9. Fields and Validations Listing:

1. **Customer Code** (type: string, required, max: 10 characters).
2. **Name** (type: string, required, max: 50 characters).
3. **Email** (type: string, optional, valid email format).
4. **Country Code** (type: string, required, predefined list).

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as no specific flowchart is provided in the code.)

### Sequence Diagram:
(Not applicable as no specific sequence diagram is provided in the code.)

### Code Snippets:
```pascal
procedure TFORMLcustomer.BTNokClick(Sender: TObject);
begin
  // Save customer data
end;
```

### Screenshots:
(Not applicable as no DFM file is provided.)

---

## 11. Important Comments in the Code:

- `FormShow`: Critical for initializing the form and loading data.
- `ACTaddressAndDocExecute`: Handles address and document management.

---

## 12. Conclusion:

The `Lcustomer` code unit provides a comprehensive interface for managing customer data. It is well-structured and integrates advanced search and management functionalities. However, it could benefit from more detailed error handling and validation logic.

---

## 13. Short Summary:

The `Lcustomer` code unit is a customer management interface with grid display, search functionality, and detailed customer data management. It supports creating, modifying, and viewing customer records, with advanced search and portal information management.#### **Lcustomer.pas**

```
unit Lcustomer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, Buttons, sSpeedButton, kneFRGridManager,
  ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter, kneCBList,
  sDBText, sScrollBox, kneFRfindCriteria, kneEnterAsTab, FRfindCriteriaCustomer,
  FRfindCriteriaListCustomer, knePrivileges, cxGridDBDataDefinitions,
  sButton, sEdit, sCheckBox, sMemo, sPageControl, kneFREditSOA,
  kneFRGridEditSOA, FRcustCrosssellBrand, sDBCheckBox, cxContainer,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, sComboBox;

type
  TFORMLcustomer = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    DBTXTcode: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    DBTXTabbreviatedName: TsDBText;
    DBTXTname: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBTXTlanguageCode: TsDBText;
    sLabel3: TsLabel;
    sBevel4: TsBevel;
    DBTXTcountryCode: TsDBText;
    DBTXTcountryDesc: TsDBText;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    ACTaddressAndDoc: TAction;
    PNL1: TsPanel;
    BTNaddress: TsBitBtn;
    cxEDTvalidVAT: TcxEditRepositoryImageComboBoxItem;
    IMLvat: TImageList;
    FRAMEfindCriteriaListCustomer1: TFRAMEfindCriteriaListCustomer;
    PNLchgPortalInfo: TsPanel;
    BTNchgPortalInfo: TsBitBtn;
    ACTchgPortalInfo: TAction;
    PNLportalInfo: TsPanel;
    SRBportalInfo: TsScrollBox;
    LBLremarks: TsLabel;
    LBLecommerceEmail: TsLabel;
    EDTecommerceEmail: TsEdit;
    EDTremarks: TsEdit;
    PNLbtns: TsPanel;
    BTNcancel: TsButton;
    BTNok: TsButton;
    CBXportalFlag: TsCheckBox;
    CBXallowRappel: TsCheckBox;
    CBXallowEnquires: TsCheckBox;
    CBXallowRfq: TsCheckBox;
    CBXallowClaims: TsCheckBox;
    CBXallowMkt: TsCheckBox;
    LBLclientAggr: TsLabel;
    EDTclientAggr: TsEdit;
    FRAMEcustCrosssellBrand1: TFRAMEcustCrosssellBrand;
    PGCportalInfo: TsPageControl;
    TSHportalInfoPrincipal: TsTabSheet;
    TSHcrossSellingBrands: TsTabSheet;
    CHKallowCrosssell: TsCheckBox;
    LBLchannelSource: TsLabel;
    CBOchannelSource: TcxImageComboBox;
    CBXallowBudget: TsCheckBox;
    CBXallowAllocation: TsCheckBox;
    CBXallowPriceList: TsCheckBox;
    CBXallowPrices: TsCheckBox;
    CBXallowStock: TsCheckBox;
    sLabel4: TsLabel;
    EDTcompanyAggregator: TsEdit;
    CBXallowSalesOut: TsCheckBox;
    CBXallowOtherPay: TsCheckBox;
    CBXallowRewards: TsCheckBox;
    procedure FormShow(Sender: TObject);
    procedure ACTaddressAndDocExecute(Sender: TObject);
    procedure BTclearCriteriaClick(Sender: TObject);
    procedure ACTchgPortalInfoExecute(Sender: TObject);
    procedure CDSlistAfterScroll(DataSet: TDataSet);
    procedure BTNokClick(Sender: TObject);
    procedure BTNcancelClick(Sender: TObject);
    procedure cxDBVmasterFocusedRecordChanged(
      Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord;
      ANewItemRecordFocusingChanged: Boolean);
    procedure PGCportalInfoChange(Sender: TObject);
  private
```

#### **Lcustomer.dfm**

```
inherited FORMLcustomer: TFORMLcustomer
  Left = 433
  Top = 152
  Caption = 'Customers List'
  ClientHeight = 686
  ClientWidth = 1078
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 225
    Width = 1078
  end
  inherited PNLsearchArea: TsPanel
    Width = 1078
    Height = 181
    inherited PNLsearchButtons: TsPanel
      Left = 984
      Height = 179
      TabOrder = 0
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      Width = 983
      Height = 179
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        Width = 979
        Height = 175
        inline FRAMEfindCriteriaListCustomer1: TFRAMEfindCriteriaListCustomer
          Left = 1
          Top = 1
          Width = 977
          Height = 173
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited sPageControl1: TsPageControl
            Width = 977
            Height = 173
            inherited SHcriteria: TsTabSheet
              inherited sLabel2: TsLabel
                FocusControl = FRAMEfindCriteriaListCustomer1.FRAMEfindCustMarket.FE
              end
              inherited Label10: TsLabel
                FocusControl = FRAMEfindCriteriaListCustomer1.FRAMEfindGroup.FE
              end
              inherited LBL1: TsLabel
                FocusControl = FRAMEfindCriteriaListCustomer1.FRAMEfindSeller.FE
              end
              inherited CBObusChannel: TcxImageComboBox
                Width = 122
              end
              inherited CBOstat: TcxImageComboBox
                Width = 99
              end
            end
          end
        end
      end
    end
  end
  inherited PNLactions: TsPanel
    Width = 1078
    inherited CLBlistActions: TsCoolBar
      Width = 1076
      Bands = <
        item
          Break = False
          Control = PNLstandardActions
          ImageIndex = -1
          MinHeight = 40
          Width = 1072
        end>
      inherited PNLstandardActions: TsPanel
        Width = 1059
        inherited BTNseparator: TsSpeedButton
          Width = 9
        end
        inherited PNLformActions: TsPanel
          Left = 1012
        end
        object PNL1: TsPanel
          Left = 502
          Top = 1
          Width = 83
          Height = 38
          Align = alLeft
```
<!-- tabs:end -->

