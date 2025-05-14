<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar unidades de pedidos de clientes. Ele fornece uma grade (grid) para exibir, editar e interagir com os dados relacionados às unidades de pedidos. O objetivo principal é facilitar a manipulação e visualização de dados de unidades de pedidos de clientes em um formato estruturado e interativo.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do componente.
  - Componentes visuais como `TcxGrid`, `TcxGridDBTableView` para exibição de dados em formato de grade.
  - Integração com serviços SOAP para manipulação de dados remotos.
  - Uso de bibliotecas auxiliares como `kneUtils`, `kneFindDialog`, e `kneDialogFactory`.

* **Forma do Componente:**
  - **Grade de Exibição (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `businessUnit` (string): Unidade de negócio.
      - `formatOrc` (string): Formato do orçamento.
      - `unitCd` (string): Código da unidade.
      - `unitDesc` (string): Descrição da unidade.
    - **Ações da Grade e seus Efeitos:**
      - Edição de valores diretamente na grade.
      - Busca de valores em campos específicos utilizando diálogos de busca.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar novas unidades de pedidos.
  - Editar valores diretamente na grade.
  - Buscar e preencher valores em campos específicos utilizando diálogos de busca.

* **Componentes Principais:**
  - `TcxGrid`: Exibe os dados em formato de grade.
  - `TcxGridDBTableView`: Permite a edição e interação com os dados.
  - `TsBitBtn`: Botões para ações como adicionar, aplicar e cancelar.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `se botão adicionar for clicado então execute ACTaddExecute`.
  - Evento `OnEditValueChanged` da grade: `se valor de célula for alterado então execute cxDBVtableEditValueChanged`.
  - Evento `OnButtonClick` do campo de busca: `se botão de busca for clicado então execute m_SetFindUnitCd`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização: O componente é criado e configurado no construtor `Create`. As propriedades da grade e os campos visíveis são definidos.
  - Interação do Usuário: O usuário pode clicar em botões para adicionar ou aplicar alterações, ou editar diretamente os valores na grade.
  - Funções Executadas:
    - `ACTaddExecute` (arquivo atual): Adiciona uma nova unidade.
    - `cxDBVtableEditValueChanged` (arquivo atual): Atualiza valores na grade.
    - `m_SetFindUnitCd` (arquivo atual): Abre um diálogo de busca para preencher campos.

* **Dados Necessários:**
  - Código do cliente (`customerCode`).
  - Informações das unidades (`unitCd`, `unitDesc`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Adicionar": Disponível sempre que o componente está ativo.
  - Botão "Aplicar": Disponível após alterações na grade.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Campo `unitCd`: Utiliza um diálogo de busca para validação e preenchimento.

---

## 5. Funções Principais:

* **ACTaddExecute:** Adiciona uma nova unidade de pedido.
* **cxDBVtableEditValueChanged:** Atualiza os valores na grade quando editados.
* **m_SetFindUnitCd:** Abre um diálogo de busca para preencher o campo `unitCd`.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `kneDialogFactory`.
  - Propósito: Abrir diálogos de busca para preencher campos.
  - Dados Enviados: Não especificado.
  - Dados Recebidos: Não especificado.
  - Tratamento de Erros: Não especificado.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`, `kneFindDialog`, `kneDialogFactory`: Utilizadas para funcionalidades auxiliares como busca e manipulação de dados.
  - `cxGrid`, `cxGridDBTableView`: Componentes visuais para exibição de dados.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base herdada para funcionalidades de edição em grade.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `businessUnit` (string): Não definido no código.
  - `formatOrc` (string): Não definido no código.
  - `unitCd` (string): Validação via diálogo de busca.
  - `unitDesc` (string): Não definido no código.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `unitCd` → `priceUnitCode`.
  - `unitDesc` → `descrip`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```pascal
  procedure TFRAMEcustOrdUnits.ACTaddExecute(Sender: TObject);
  begin
    // Lógica para adicionar uma nova unidade
  end;
  ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades no construtor `Create`:
  ```pascal
  MasterKeyFields := 'customerCode=custCd';
  DataPacketName := 'CustomerUnits';
  PropertyName := 'customerUnits';
  FrameType := frtDetail;
  ```

* Configuração de campos visíveis na grade:
  ```pascal
  DefineOrderFields(mc_GRID_FIELDS);
  ```

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar unidades de pedidos de clientes, com funcionalidades de edição e busca integradas. No entanto, faltam definições explícitas de validações, mensagens de erro e valores padrão, o que pode limitar a usabilidade em cenários mais complexos.

---

## 13. Resumo Curto:

O código implementa uma interface de grade para gerenciar unidades de pedidos de clientes, permitindo edição e busca de dados. Ele utiliza componentes visuais e integrações SOAP para manipulação de dados.#### **FRcustOrdUnits.pas**

```
unit FRcustOrdUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneTypes;

type
  TFRAMEcustOrdUnits = class(TFRAMEBaseGridEditSOA)
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_SetFindUnitCd(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeUnitCd(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

  end;

var
  FRAMEcustOrdUnits: TFRAMEcustOrdUnits;

implementation

uses
  kneUtils, kneFindDialog, kneDialogFactory, kneConfigObjects, kneFGFindUtils,
  PriceUnitsByFormatsUtils;

const
  mc_GRID_FIELDS = 'businessUnit;formatOrc;unitCd;unitDesc';

{$R *.dfm}

constructor TFRAMEcustOrdUnits.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=custCd';
  DataPacketName := 'CustomerUnits';
  PropertyName := 'customerUnits';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
//    DefineReadOnlyFields('');
    DefineOrderFields(mc_GRID_FIELDS);

		// Custom Editors ..........................................................
    AddCustomField('unitCd','cxEDTfind');

  end; //with

  cxEDTfind.Properties.OnButtonClick := m_SetFindUnitCd;
end;


procedure TFRAMEcustOrdUnits.m_SetFindUnitCd(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin

  try
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find do
    begin
      with Options.DataSelection do
      begin
        FieldNameForCode := 'priceUnitCode';
        FieldNamesForDesc.Clear;
        FieldNamesForDesc.Add('descrip');

        TargetDataSet := cxDBVtable.DataController.DataSource.DataSet;
        TargetFieldNameForCode := 'unitCd';
        TargetFieldNamesForDesc.Clear;
        TargetFieldNamesForDesc.Add('unitDesc');

        UseTargetDataSet:= True;
```

#### **FRcustOrdUnits.dfm**

```
inherited FRAMEcustOrdUnits: TFRAMEcustOrdUnits
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Left = 2
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
```
<!-- tabs:end -->

